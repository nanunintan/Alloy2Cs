{-# LANGUAGE FlexibleContexts #-}

module Transform.Alloy2OCL (alloy2ocl) where

import Control.Monad.State
import Control.Monad.Error
import Data.List (intersect,groupBy,nub,sortBy)
import Data.Set as Set (member,fromList)
import Data.Map as Map hiding (map,member,filter,foldr,foldl)
import Data.Ord (comparing)
import Prelude hiding (EQ)
import Data.Generics
import System.IO
import Debug.Trace

import Alloy.AST as Alloy
import Alloy.Types
import Alloy.Parser hiding (expr)
import Alloy.Relation as Rel
import OCL.AST as OCL
import OCL.Pretty
import OCL.Simplification
import qualified OCL.Types
import Transform.Alloy2CD

data S = S {
      env  :: Env,
      info :: Meta,
      self :: String,
      seed :: Int
    }

test :: String -> IO ()
test f = do alloy <- parseFile f
            putStrLn $ show alloy
            case alloy2ocl alloy of Left error -> putStrLn $ "Error: " ++ error
                                    Right ocl  -> putStrLn (OCL.Pretty.pretty ocl)

alloy2ocl :: MonadError String m => Alloy.Module -> m OCL.Package
alloy2ocl s@(Alloy.Module (Just m) [] p) = 
    do let init = S {info = meta s, env = initial (meta s), self = "", seed = 0}
       p' <- evalStateT (sequence $ map para $ filter aux p) init
       let m' = alloy2cd s
       let o = removets $ simplify $ OCL.Package m (concat p')
       o' <- liftm ("Cast fixing failed: " ++ show o) $ OCL.Types.fixspec m' o
       return $ simplify o'
    where aux (FactDecl _ _) = True
          aux (FunDecl _ _ _ _ _ _) = True
          aux (PredDecl _ _ _ _ _) = True
          aux _ = False
          liftm :: MonadError String m => String -> Maybe a -> m a
          liftm e (Just x) = return x
          liftm e Nothing = throwError e
alloy2ocl _ = throwError "Does not support module import and module must have a name"

para :: MonadError String m => Alloy.Paragraph -> StateT S m [OCL.Constraint]
para (FactDecl _ (Block fs _)) = 
    do e <- gets env
       sequence $ map aux (map (buildTypes e) fs)
    where aux (Quant QAll [Alloy.Decl False False ["t"] False (Alloy.Id "Time" _)] (Quant QAll [Alloy.Decl False False [var] False (Alloy.Id sig _)] f _) _) =
              do when (temporal var) $ throwError "Fact does not conform to local state idiom"
                 modify (\s -> s {self = var})
                 f' <- form f
                 return $ InvDecl sig f'
          aux (Quant QAll [Alloy.Decl False False [var] False (Alloy.Id sig _)] f _) =
              do when (temporal var) $ throwError "Fact does not conform to local state idiom"
                 modify (\s -> s {self = var})
                 f' <- form f
                 return $ InvDecl sig f'
          aux _ = throwError "Fact does not follow required shape"
para (FunDecl False Nothing fun (Just l) (Set (Alloy.Id r _) _) e) =
          do unless (not (Prelude.null l) && all aux l) $ throwError "Function declaration does not conform to local state idiom"
             let d' = concat $ map (\(Alloy.Decl _ _ xs _ (Alloy.Id c _)) -> map (\x -> OCL.Decl x c) xs) l
             let d = if (last d' == OCL.Decl "t" "Time") then init d' else d'
             when (Prelude.null d) $ throwError "Function declaration does not conform to local state idiom"
             let OCL.Decl x c = head d
             modify (\s -> s {self = x})
             y <- fresh
             g <- gets env
             let g' = foldl ins g l
             e' <- expr [y] (buildTypes g' e)
             return $ [QueryDecl c fun (tail d) r (select [r] y e')]
    where aux (Alloy.Decl False False _ False (Alloy.Id _ _)) = True
          aux _ = False
          ins e (Alloy.Decl False disj vars False exp) = updt e vars (inferType e exp)
          updt e vs (Just (Relation t)) = foldr (\v env -> Map.insert v t $ filterWithKey (\k _ -> not $ overloaded k v) env) e vs
          updt e vs _ = e
para (FunDecl _ _ _ _ _ _) = throwError "Function declaration does not conform to local state idiom"
para (PredDecl False Nothing pred (Just l) (Block es _)) =
          do unless (all aux l) $ throwError "Predicate declaration does not conform to local state idiom"
             let d = concat $ map (\(Alloy.Decl _ _ xs _ (Alloy.Id c _)) -> map (\x -> OCL.Decl x c) xs) l
             unless (length d > 2 && last d == (OCL.Decl "t'" "Time") && last (init d) == (OCL.Decl "t" "Time")) $ throwError "Predicate declaration does not conform to local state idiom"
             let OCL.Decl x c = head d
             modify (\s -> s {self = x})
             g <- gets env
             let g' = foldl ins g l
             let es' = map (buildTypes g') es
             let pre = filter (not . ispos) es'
             let pos = filter ispos es'
             pre' <- sequence $ map form pre
             pos' <- sequence $ map form pos
             return $ [OpDecl c pred (tail (init (init d))) pre' pos']
    where aux (Alloy.Decl False False _ False (Alloy.Id _ _)) = True
          aux _ = False
          ins e (Alloy.Decl False disj vars False exp) = updt e vars (inferType e exp)
          updt e vs (Just (Relation t)) = foldr (\v env -> Map.insert v t $ filterWithKey (\k _ -> not $ overloaded k v) env) e vs
          updt e vs _ = e
          ispos :: Alloy.Expr -> Bool
          ispos e = everything (||) (mkQ False q) e
          q (Alloy.Id "t'" _) = True
          q _ = False
para (PredDecl _ _ _ _ _) = throwError "Predicate declaration does not conform to local state idiom"

form :: MonadError String m => Alloy.Expr -> StateT S m OCL.Expr
form (Alloy.Equals l r t) = 
    do a <- getT l
       case a of Int -> do l' <- iexpr l
                           r' <- iexpr r
                           return $ Comp OCL.EQ l' r'
                 Relation _ -> form (Alloy.And (Alloy.In l r t) (Alloy.In r l t) (Just Bool))
form (Alloy.In l r _) = 
    do Relation a <- getT l
       n <- getA a
       vs <- sequence $ replicate n fresh
       let ts = map (project a) [1..n]
       l' <- expr vs l
       r' <- expr vs r
       m <- gets info
       return $ foldr (\(v,t) e -> forall (reduce (parent m) t) v e) (OCL.Implies l' r') $ zip vs ts
form (Alloy.Quant _ [] e _) = form e
form (Alloy.Quant q (Alloy.Decl False False [x] False t:qs) e (Just Bool)) = 
    do Relation a <- getT t
       n <- getA a
       unless (n==1) $ throwError "Higher order quantification"
       m <- gets info
       let c = reduce (parent m) $ project a 1
       e' <- form $ Alloy.Quant q qs e (Just Bool)
       t' <- expr [x] t
       return $ case q of QAll  -> forall c x (OCL.Implies t' e')
                          QSome -> exists c x (OCL.Implies t' e')
                          QNo   -> IsEmpty $ select c x (OCL.Implies t' e')
                          QOne  -> Comp OCL.EQ (Size (select c x (OCL.Implies t' e'))) (OCL.Num 1)
form (Alloy.Lone e _) = form (Alloy.LTE (Cardinality e (Just Int)) (Alloy.Num 1 (Just Int)) (Just Bool))
form (Alloy.Some e _) = form (Alloy.GTE (Cardinality e (Just Int)) (Alloy.Num 1 (Just Int)) (Just Bool))
form (Alloy.One e _) = form (Alloy.Equals (Cardinality e (Just Int)) (Alloy.Num 1 (Just Int)) (Just Bool))
form (Alloy.No e _) = form (Alloy.Equals (Cardinality e (Just Int)) (Alloy.Num 0 (Just Int)) (Just Bool))
form (Alloy.LTE l r _) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Comp OCL.LTE l' r'
form (Alloy.GTE l r _) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Comp OCL.GTE l' r'
form (Alloy.Not e _) = 
    do e' <- form e
       return $ OCL.Not e'
form (Alloy.Implies l r _) =
    do l' <- form l
       r' <- form r
       return $ OCL.Implies l' r'
form (Alloy.And l r _) =
    do l' <- form l
       r' <- form r
       return $ OCL.And l' r'
form (Alloy.Or l r _) =
    do l' <- form l
       r' <- form r
       return $ OCL.Or l' r'
form e = error $ "Formula translation missing case: " ++ show e

expr :: MonadError String m => [OCL.Name] -> Alloy.Expr -> StateT S m OCL.Expr
expr t (Alloy.Id n (Just (Relation a))) = 
    do m <- gets info
       s <- gets self
       if member n (sigs m)
       then if member n (consts m)
            then let p = parent m ! n
                 in  return $ Comp EQ (OCL.Id (head t)) (OCL.Const n p)
            else return $ includes (reduce (parent m) (project a 1)) (head t)
       else if n `elem` (map fst (rels m))
            then return $ navigate t (rename n)
            else if n `elem` (map fst (funs m))
                 then return $ funcall t n
                 else if n == s
                      then return $ Comp EQ (OCL.Id (head t)) (OCL.Id "self")
                      else return $ Comp EQ (OCL.Id (head t)) (OCL.Id n)      
    where rename s | '.' `elem` s = tail $ dropWhile (/='.') s
                   | otherwise = s                        
expr vs (Join r s _) = 
    do Relation a <- getT r
       Relation b <- getT s
       n <- getA a
       let t = intersect (project a n) (project b 1)
       v <- fresh
       r' <- expr (take (n-1) vs ++ [v]) r
       s' <- expr ([v] ++ drop (n-1) vs) s
       m <- gets info
       return $ exists (reduce (parent m) t) v (OCL.And  r' s')
expr vs (None _) = return Bot
expr vs (Univ _) = return Top
expr [y1,y2] (Iden _) = return $ Comp OCL.EQ (OCL.Id y1) (OCL.Id y2)
expr vs (Alloy.Intersection r s _) =
    do r' <- expr vs r
       s' <- expr vs s
       return $ OCL.And r' s'
expr vs (Alloy.Plus r s (Just (Relation _))) =
    do r' <- expr vs r
       s' <- expr vs s
       return $ OCL.Or r' s'
expr vs (Alloy.Minus r s (Just (Relation _))) =
    do r' <- expr vs r
       s' <- expr vs s
       return $ OCL.And r' (OCL.Not s')
expr vs (Alloy.Arrow r Nothing Nothing s _) =
    do Relation a <- getT r
       n <- getA a
       r' <- expr (take n vs) r
       s' <- expr (drop n vs) s
       return $ OCL.And r' s'
expr vs (Converse e _) = expr (reverse vs) e
expr vs (RanRestriction r s _) =
    do r' <- expr vs r
       s' <- expr [last vs] s
       return $ OCL.And r' s'
expr vs (DomRestriction r s _) =
    do s' <- expr vs s
       r' <- expr [head vs] r
       return $ OCL.And r' s'
expr [y1,y2] (RTransClosure e t) = 
    do e' <- expr [y1,y2] (TransClosure e t)
       return (OCL.Or (Comp OCL.EQ (OCL.Id y1) (OCL.Id y2)) e')
expr [y1,y2] (TransClosure e _) =
    do Relation a <- getT e
       n <- getA a
       m <- gets info
       let t = reduce (parent m) $ project a 2
       z1 <- fresh
       z2 <- fresh
       e' <- expr [z1,z2] e
       return $ Includes (Closure (OCL.Id y1) z1 (select t z2 e')) (OCL.Id y2)
expr vs (Comprehension d l _) =
    do let d' = Prelude.map (\(Alloy.Decl _ _ [x] _ e) -> (x,e)) d
       es <- sequence $ Prelude.map (\(v,e) -> expr [v] e) (zip vs (Prelude.map snd d'))
       let es' = Prelude.map (\a -> foldr (\(x,y) e -> replace y (OCL.Id x) e) a (zip vs (map fst d'))) es
       l' <- form l
       let l'' = foldr (\(x,y) e -> replace y (OCL.Id x) e) l' (zip vs (map fst d'))
       return $ foldr OCL.And l'' es'
expr _ e = error $ "Expression translation missing case: " ++ show e

iexpr :: MonadError String m => Alloy.Expr -> StateT S m OCL.Expr
iexpr (Alloy.Num n _) = return $ OCL.Num n
iexpr (Cardinality e _) = 
    do Relation a <- getT e
       n <- getA a
       y <- fresh
       m <- gets info
       let t = project a 1
       if n == 1 
       then do e' <- expr [y] e
               return $ Size $ select (reduce (parent m) t) y e'
       else if n >= 1
            then do let t' = Rel.fromList (map (:[]) t)
                    e' <- iexpr (Cardinality (Join (Alloy.Id y (Just (Relation t'))) e (Just (Relation (Rel.join t' a)))) (Just Int))
                    return $ collect (reduce (parent m) t) y e'
            else return $ OCL.Num 0
iexpr e = error $ "Integer expression translation missing case: " ++ show e


navigate :: [OCL.Name] -> OCL.Name -> OCL.Expr
navigate (s:[]) r = Navigate False (OCL.Id s) r []
navigate (s:l)  r = Includes (Navigate False (OCL.Id s) r (map OCL.Id (init l))) (OCL.Id (last l))

funcall :: [OCL.Name] -> OCL.Name -> OCL.Expr
funcall (s:[]) r = Call False (OCL.Id s) r []
funcall (s:l)  r = Includes (Call False (OCL.Id s) r (map OCL.Id (init l))) (OCL.Id (last l))

forall :: [OCL.Name] -> OCL.Name -> OCL.Expr -> OCL.Expr
forall cs x f = foldr1 OCL.And $ map (\c -> (ForAll (AllInstances c) x f)) cs

exists :: [OCL.Name] -> OCL.Name -> OCL.Expr -> OCL.Expr
exists cs x f = foldr1 OCL.Or $ map (\c -> (Exists (AllInstances c) x f)) cs

select :: [OCL.Name] -> OCL.Name -> OCL.Expr -> OCL.Expr
select cs x f = foldr1 OCL.Union $ map (\c -> (Select (AllInstances c) x f)) cs

collect :: [OCL.Name] -> OCL.Name -> OCL.Expr -> OCL.Expr
collect cs x f = foldr1 OCL.Plus $ map (\c -> (Sum (Collect (AllInstances c) x f))) cs


includes :: [OCL.Name] -> OCL.Name -> OCL.Expr
--includes cs x = foldr1 OCL.Or $ map (\c -> (Includes (AllInstances c) (OCL.Id x))) cs
includes cs x = foldr1 OCL.Or $ map (\c -> (OclIsKindOf (OCL.Id x) c)) cs

temporal :: Alloy.Name -> Bool
temporal "t" = True
temporal "t'" = True
temporal _ = False

fresh :: MonadError String m => StateT S m String
fresh = do i <- gets seed
           modify (\s -> s {seed = i+1})
           return $ "v" ++ show i

getT :: MonadError String m => Alloy.Expr -> m Type
getT e = case getType e of Just r -> return r
                           Nothing -> throwError $ "Typing error:" ++ show e

getA :: MonadError String m => Relation Alloy.Name -> m Int
getA e = case arity e of Just r -> return r
                         Nothing -> throwError $ "Typing error:" ++ show e

reduce :: Map Alloy.Name Alloy.Name -> [Alloy.Name] -> [Alloy.Name]
reduce m l = fix l []
    where fix l a | l == a = l
                    | otherwise = fix (nub (concat (map aux $ groupBy (\x y -> m!x == m!y) (sortBy (comparing (m!)) l)))) l
          aux l = let p = m ! head l
                      f = keys $ fst $ Map.partition (==p) m
                  in  if (p /= "univ" && (Set.fromList l) == (Set.fromList f)) then [p] else l

removets :: OCL.Package -> OCL.Package
removets = everywhere (mkT t)
    where t (Includes (Navigate _ e r []) (OCL.Id "t")) = Navigate True e r [] 
          t (Includes (Navigate _ e r l)  (OCL.Id "t")) = Includes (Navigate True e r (init l)) (last l)
          t (Includes (Navigate _ e r []) (OCL.Id "t'")) = Navigate False e r [] 
          t (Includes (Navigate _ e r l)  (OCL.Id "t'")) = Includes (Navigate False e r (init l)) (last l)
          t (Call _ e f l) | last l == OCL.Id "t" = Call True e f (init l)
          t (Call _ e f l) | last l == OCL.Id "t'" = Call False e f (init l)
          t e = e