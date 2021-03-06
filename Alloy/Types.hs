module Alloy.Types (buildTypes, Meta (..), Env, initial, meta, getType, inferType, overloaded) where

import Data.Set as Set hiding (foldl,foldr)
import Data.Map as Map hiding (foldl,foldr)
import Data.List (isSuffixOf)
import Data.Generics hiding (empty,GT)
import Data.Maybe as Maybe
import Text.ParserCombinators.Parsec
import Control.Monad
import Prelude hiding (LT,GT)

import Alloy.AST
import Alloy.Parser
import Alloy.Relation as Rel

data Meta = Meta { name     :: Name
                 , sigs     :: Set Name
                 , abstract :: Set Name
                 , consts   :: Set Name
                 , parent   :: Map Name Name
                 , rels     :: [(Name,Expr)]
                 , funs     :: [(Name,Expr)]
                 , preds    :: [(Name,Expr)]
                 } deriving Show

meta :: Module -> Meta
meta s@(Module (Just n) [] _) = 
    Meta { name = n 
         , sigs     = Set.insert "univ" $ everything Set.union (mkQ Set.empty f) s 
         -- real abstract sigs, i.e., must be extended
         , abstract = Set.insert "univ" $ everything Set.union (mkQ Set.empty h) s `Set.intersection` Set.fromList (Map.elems aux)
         , consts = everything Set.union (mkQ Set.empty k) s
         -- parent includes remainder signatures
         , parent   = let nabs = Set.toList $ (Set.fromList $ Map.elems aux) `Set.difference` abstract (meta s)
                      in  aux `Map.union` Map.fromList (zip (Prelude.map ('$':) nabs) nabs)
         , rels     = everything (++) (mkQ [] i) s
         , funs     = everything (++) (mkQ [] j) s
         , preds    = everything (++) (mkQ [] l) s}
       where aux :: Map Name Name
             aux = everything Map.union (mkQ Map.empty g) s
             f :: Paragraph -> Set Name
             f (SigDecl _ s TopLevel _ _) = Set.fromList s
             f (SigDecl _ s (Extends _) _ _) = Set.fromList s
             f (EnumDecl s c) = Set.insert s $ Set.fromList c
             f _ = Set.empty
             h :: Paragraph -> Set Name
             h (SigDecl q s TopLevel _ _) | Abstract `elem` q = Set.fromList s
             h (SigDecl q s (Extends _) _ _) | Abstract `elem` q = Set.fromList s
             h (EnumDecl s c) = Set.singleton s
             h _ = Set.empty
             k :: Paragraph -> Set Name
             k (EnumDecl s c) = Set.fromList c
             k _ = Set.empty
             g :: Paragraph -> Map Name Name
             g (SigDecl _ a TopLevel _ _) = Map.fromList $ zip a (repeat "univ")
             g (SigDecl _ a (Extends b) _ _) = Map.fromList $ zip a (repeat b)
             g (EnumDecl s c) = Map.insert s "univ" $ Map.fromList $ zip c (repeat s)
             g _ = Map.empty
             i :: Paragraph -> [(Name,Expr)] 
             i (SigDecl _ a TopLevel d _) = concat $ [concatMap (\(Decl _ _ n _ e) -> zip (Prelude.map ((s++".")++) n) (repeat $ Arrow (Id s Nothing) Nothing (mult e) (rmult e) Nothing)) d | s <- a]
             i (SigDecl _ a (Extends _) d _) = concat $ [concatMap (\(Decl _ _ n _ e) -> zip (Prelude.map ((s++".")++) n) (repeat $ Arrow (Id s Nothing) Nothing (mult e) (rmult e) Nothing)) d | s <- a]
             i (SigDecl [] s (Inside t) [] Nothing) = Prelude.map (\n -> (n,(Id t Nothing))) s
             i _ = []
             j :: Paragraph -> [(Name,Expr)]
             j (FunDecl _ Nothing n Nothing e _) = [(n,e)]
             j (FunDecl _ Nothing n (Just d) e _) = [(n, prod $ Prelude.map (\(Decl _ _ l _ a) -> prod $ Prelude.map (const a) l) d ++ [e])]
             j _ = []
             l :: Paragraph -> [(Name,Expr)]
             l (PredDecl _ Nothing n Nothing _) = [(n,None Nothing)]
             l (PredDecl _ Nothing n (Just d) _) = [(n, prod $ Prelude.map (\(Decl _ _ l _ a) -> prod $ Prelude.map (const a) l) d)]
             l _ = []
             mult (Some e _) = Just MSome
             mult (Lone e _) = Just MLone
             mult (Set e _) = Nothing
             mult (One e _) = Just MOne
             mult (Arrow _ _ _ _ _) = Nothing
             mult e = Just MOne
             rmult (Some e _) = e
             rmult (Set e _)  = e
             rmult (One e _)  = e
             rmult (Lone e _) = e
             rmult e = e
             prod :: [Expr] -> Expr
             prod = foldr1 (\l r -> Arrow l Nothing Nothing r Nothing) . Prelude.map rmult
                   
type Env = Map Name (Relation Name)

-- Does not work correctly with declarations mentioning relations of the same signature
-- Does not work properly with higher order neither function parameters that mention other parameters
initial :: Meta -> Env
initial m = let senv = Map.fromList $ Prelude.map (\s -> (s,Rel.fromList $ Prelude.map (:[]) $ stype s)) (Set.toList $ sigs m)
                renv = foldl (\e (n,t) -> Map.insert n (case inferType e t of Just (Relation r) -> r) e) senv (rels m)
            in  foldl (\e (n,t) -> Map.insert n t e) renv $ Prelude.map (\(f,t) -> (f, case inferType renv t of Just (Relation r) -> r)) (funs m)
    where stype :: Name -> [Name]
          stype s | s `elem` Map.elems (parent m) = concatMap stype $ Map.keys $ Map.filter (==s) (parent m)
                  | otherwise = [s]

getType :: Expr -> Maybe Type
getType = fromJust . gfindtype

changeType :: Expr -> (Type -> Type) -> Expr
changeType e f = fromJust $ somewhere (gmapMo (mkMp aux)) e
                 where aux :: Maybe Type -> Maybe (Maybe Type)
                       aux t = Just (fmap f t)

bounding :: Env -> Expr -> Expr
bounding e (Id n _) = let ns = Prelude.filter (overloaded n) (keys e)
                       in  if Prelude.null ns then Id n Nothing
                           else foldr1 (\l r -> Plus l r (do Relation a <- getType l; 
                                                             Relation b <- getType r; 
                                                             return $ Relation $ Rel.union a b)) 
                                       (Prelude.map (\n -> Id n $ fmap Relation (Map.lookup n e)) ns)
bounding e (Num n _) = Num n $ Just Int
bounding e (None _) = None $ Just $ Relation $ Set.fromList [[]] -- This is not ok
bounding e (Iden _) = Univ $ do u <- Map.lookup "univ" e
                                return $ Relation $ Rel.fromList $ Prelude.map (\[x] -> [x,x]) $ Rel.toList u
bounding e (Univ _) = Univ $ fmap Relation (Map.lookup "univ" e)
bounding e (Converse l t) = Converse (bounding e l) $ do Relation l' <- getType (bounding e l)
                                                         return $ Relation $ converse l'
bounding e (TransClosure l t) = TransClosure (bounding e l) $ do Relation l' <- getType (bounding e l)
                                                                 a <- arity l'
                                                                 guard $ a == 2
                                                                 return $ Relation $ transclosure l'
bounding e (RTransClosure l t) = RTransClosure (bounding e l) $ getType $ bounding e $ Plus (TransClosure l t) (Iden Nothing) Nothing
-- This does not work well with overloading, since the arity of an expression might not be uniform
bounding e (Join l r t) = Join (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                  a <- arity l'
                                                                  Relation r' <- getType (bounding e r)
                                                                  b <- arity r'
                                                                  guard $ a+b-2 > 0 
                                                                  return $ Relation $ Rel.join l' r'
bounding e (BoxJoin r l t) = bounding e $ foldr (\x y -> Join x y Nothing) r (reverse l) 
bounding e (Arrow l m n r t) = Arrow (bounding e l) m n (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                            Relation r' <- getType (bounding e r)
                                                                            return $ Relation $ arrow l' r'
bounding e (Plus l r t) = Plus (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                  a <- arity l'
                                                                  Relation r' <- getType (bounding e r)
                                                                  b <- arity r'
                                                                  guard $ a == b
                                                                  return $ Relation $ Rel.union l' r'
                                                               `mplus`
                                                               do Int <- getType (bounding e l)
                                                                  Int <- getType (bounding e r)
                                                                  return Int
bounding e (Minus l r t) = Minus (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                    a <- arity l'
                                                                    Relation r' <- getType (bounding e r)
                                                                    b <- arity r'
                                                                    guard $ a == b
                                                                    return $ Relation $ l'
                                                               `mplus`
                                                               do Int <- getType (bounding e l)
                                                                  Int <- getType (bounding e r)
                                                                  return Int
bounding e (Intersection l r t) = Intersection (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                                  a <- arity l'
                                                                                  Relation r' <- getType (bounding e r)
                                                                                  b <- arity r'
                                                                                  guard $ a == b
                                                                                  return $ Relation $ Rel.intersection l' r'
bounding e (RanRestriction l r _) =
    let l' = bounding e l
        r' = bounding e r
     in RanRestriction l' r' $ do Relation a <- getType l'
                                  n <- arity a
                                  Relation b <- getType r'
                                  m <- arity b
                                  guard (m==1)
                                  return $ Relation $ Rel.ranrestriction a b
bounding e (DomRestriction l r _) =
    let l' = bounding e l
        r' = bounding e r
     in RanRestriction l' r' $ do Relation a <- getType l'
                                  n <- arity a
                                  Relation b <- getType r'
                                  m <- arity b
                                  guard (n==1)
                                  return $ Relation $ Rel.domrestriction a b
bounding e (Cardinality l t) = Cardinality (bounding e l) $ do Relation l' <- getType (bounding e l)
                                                               return $ Int
bounding e (No l t) = No (bounding e l) $ do Relation _ <- getType (bounding e l)
                                             return $ Bool
bounding e (Some l t) = Some (bounding e l) $ do Relation _ <- getType (bounding e l)
                                                 return $ Bool
bounding e (Lone l t) = Lone (bounding e l) $ do Relation _ <- getType (bounding e l)
                                                 return $ Bool
bounding e (One l t) = One (bounding e l) $ do Relation _ <- getType (bounding e l)
                                               return $ Bool
bounding e (Set l t) = error "Can only be used in relation declaration" -- Maybe types should be non-deterministic instead of partial..
bounding e (In l r t) = In (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                              a <- arity l'
                                                              Relation r' <- getType (bounding e r)
                                                              b <- arity r'
                                                              guard $ a==b
                                                              return $ Bool
bounding e (Equals l r t) = Equals (bounding e l) (bounding e r) $ do Relation l' <- getType (bounding e l)
                                                                      a <- arity l'
                                                                      Relation r' <- getType (bounding e r)
                                                                      b <- arity r'
                                                                      guard $ a==b
                                                                      return $ Bool
bounding e (LT l r t) = LT (bounding e l) (bounding e r) $ do Int <- getType (bounding e l)
                                                              Int <- getType (bounding e r)
                                                              return $ Bool
bounding e (GT l r t) = GT (bounding e l) (bounding e r) $ do Int <- getType (bounding e l)
                                                              Int <- getType (bounding e r)
                                                              return $ Bool
bounding e (LTE l r t) = LTE (bounding e l) (bounding e r) $ do Int <- getType (bounding e l)
                                                                Int <- getType (bounding e r)
                                                                return $ Bool
bounding e (GTE l r t) = GTE (bounding e l) (bounding e r) $ do Int <- getType (bounding e l)
                                                                Int <- getType (bounding e r)
                                                                return $ Bool
bounding e (Not f _) = Not (bounding e f) $ do Bool <- getType (bounding e f)
                                               return Bool
bounding e (Implies l r _) = let l' = bounding e l
                                 r' = bounding e r
                             in  Implies l' r' $ do Bool <- getType l'
                                                    Bool <- getType r'
                                                    return Bool
bounding e (And l r _) = let l' = bounding e l
                             r' = bounding e r
                         in  And l' r' $ do Bool <- getType l'
                                            Bool <- getType r'
                                            return Bool
bounding e (Or l r _) = let l' = bounding e l
                            r' = bounding e r
                        in  Or l' r' $ do Bool <- getType l'
                                          Bool <- getType r'
                                          return Bool
bounding e (Quant q d l _) = 
    let (e',d') = decls e d
        l' = bounding e' l
    in  Quant q d' l' $ do guard $ all isRelation $ Prelude.map (\(Decl _ _ _ _ t) -> getType t) d'
                           Bool <- getType l'
                           return $ Bool
bounding e (Comprehension d l _) =
     let (e',d') = decls e d
         dt' = Prelude.map (\(Decl _ _ _ _ t) -> getType t) d'
         l' = bounding e' l
     in  Comprehension d' l' $ do guard $ all isRelation dt'
                                  Bool <- getType l'
                                  let ts = Prelude.map (\(Just (Relation a)) -> a) dt'
                                  return $ Relation $ foldr1 arrow ts
bounding e (Block l _) = let l' = Prelude.map (bounding e) l
                             t = Prelude.map getType l'
                         in  Block l' $ do x <- sequence t
                                           guard $ all (==Bool) x
                                           return Bool
bounding _ e = error $ "bounding missing case:" ++ show e


decls :: Env -> [Decl] -> (Env,[Decl])
decls e d = foldl aux (e,[]) d
    where aux (e,d) (Decl False disj vars False exp) = (updt e vars (inferType e exp),d++[(Decl False disj vars False (buildTypes e exp))])
          updt e vs (Just (Relation t)) = foldr (\v env -> Map.insert v t $ filterWithKey (\k _ -> not $ overloaded k v) env) e vs
          updt e vs _ = e

isRelation :: Maybe Type -> Bool
isRelation (Just (Relation _)) = True
isRelation _ = False

relevance :: Expr -> Expr
relevance (Intersection l r a@(Just (Relation t))) = let l' = changeType l (\(Relation p) -> Relation $ Rel.intersection p t)
                                                         r' = changeType r (\(Relation p) -> Relation $ Rel.intersection p t)
                                                     in  Intersection (relevance l') (relevance r') a
relevance (Plus l r a@(Just (Relation t))) = let l' = changeType l (\(Relation p) -> Relation $ Rel.intersection p t)
                                                 r' = changeType r (\(Relation p) -> Relation $ Rel.intersection p t)
                                             in  Plus (relevance l') (relevance r') a
relevance (Join l r a@(Just (Relation t))) = let Just (Relation p) = getType l
                                                 Just (Relation q) = getType r
                                                 p' = Rel.fromList [a | a <- Rel.toList p, b <- Rel.toList q, last a == head b, (init a ++ tail b) `elem` Rel.toList t]
                                                 q' = Rel.fromList [b | a <- Rel.toList p, b <- Rel.toList q, last a == head b, (init a ++ tail b) `elem` Rel.toList t]
                                                 l' = changeType l (\_ -> Relation $ p')
                                                 r' = changeType r (\_ -> Relation $ q')
                                             in  Join (relevance l') (relevance r') a
relevance (In l r (Just Bool)) = let Just (Relation p) = getType l
                                     Just (Relation q) = getType r
                                     r' = changeType r (\_ -> Relation $ Rel.intersection p q)
                                 in  In (relevance l) (relevance r') (Just Bool)
relevance e = gmapT (mkT relevance) e


dropIrrelevant :: Expr -> Expr
dropIrrelevant = everywhere (mkT aux)
    where aux (Plus (Id r (Just (Relation a))) (Id s b) _) | Rel.null a && overloaded r s = Id s b
          aux (Plus (Id s b) (Id r (Just (Relation a))) _) | Rel.null a && overloaded r s = Id s b
          aux e = e

overloaded :: Name -> Name -> Bool
overloaded n m = aux n == aux m 
    where aux n | '.' `elem` n = tail $ dropWhile (/='.') n
          aux n = n

buildTypes :: Env -> Expr -> Expr
buildTypes env expr = dropIrrelevant $ relevance $ bounding env expr
                    
data Error = Structural | Irrelevant | Ambiguous
             deriving Show

typeError :: Expr -> Maybe Error
typeError expr = everything mplus (mkQ Nothing aux) expr
    where aux :: Expr -> Maybe Error
          aux (None (Just (Relation s))) | Rel.null s = Nothing
          aux (Plus (Id n (Just (Relation a))) (Id m (Just (Relation b))) _) | overloaded n m && not (Rel.null a) && not (Rel.null b) = Just Ambiguous
          aux e = case getType e of Nothing -> Just Structural
                                    Just (Relation s) -> if Rel.null s then Just Irrelevant else Nothing
                                    _ -> Nothing

inferType :: Env -> Expr -> Maybe Type
inferType env expr = let texpr = buildTypes env expr
                     in  case typeError texpr of Nothing -> getType texpr
                                                 Just _  -> Nothing

test :: String -> String -> IO Expr
test f s = do spec <- parseFile f
              let env = initial (meta spec)
              case parse expr "" s of Right expr -> do putStrLn $ show $ typeError $ buildTypes env expr 
                                                       return $ buildTypes env expr
                                      Left  err  -> error $ "parse error " ++ (show err)
