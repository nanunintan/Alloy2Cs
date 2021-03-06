module OCL.Types (Meta(..),Type(..),fixspec) where

import Data.Map as Map hiding (assocs,foldr,foldl)
import Data.Set as Set hiding (foldr,foldl)
import Control.Monad
import Prelude hiding (EQ)
import Data.Generics
import Debug.Trace

import OCL.AST as OCL
import qualified CD.AST as CD

data Meta = Meta { parent :: Map Name Name } deriving Show

data Type = Class Name
          | Int
          | Bool
          | Set Type
          | Bag Type
          | Prop Name [Name] Type
            deriving (Eq,Show)

type Env = Map Name Type

initenv :: CD.Package -> Env
initenv p = Map.fromList (aux p)
    where aux :: CD.Package -> [(Name,Type)]
          aux = everything (++) (mkQ [] q)
          q (CD.Class s _ l) = everything (++) (mkQ [] (i s)) l
          q _ = []
          i s (CD.Property n _ (Just r) _ _ l) = [(n,Prop s (Prelude.map (\(CD.Qualified _ (CD.RefType x)) -> x) l) (tt r))]
          i _ _ = []
          tt (CD.RefType n) = Class n
          tt (CD.PrimitiveType "Boolean") = Bool

initmeta :: CD.Package -> Meta
initmeta p = Meta {parent = Map.fromList (aux p)}
    where aux :: CD.Package -> [(Name,Name)]
          aux = everything (++) (mkQ [] q)
          q (CD.Class n _ l) = case (everything (++) (mkQ [] i) l) of [p] -> [(n,p)]
                                                                      []  -> []
          q _ = []
          i (CD.Generalization x) = [x]
          i _ = []

subtype :: Meta -> Name -> Name -> Bool
subtype e a b | a == b = True
              | a `notElem` keys (parent e) = False
              | otherwise = subtype e (parent e ! a) b

infer :: Meta -> Env -> Expr -> Maybe Type
infer m env (Includes l r) =
    do Set (Class a) <- infer m env l
       Class b <- infer m env r
       guard (subtype m b a)
       return Bool
infer m e (ForAll l v r) =
    do Set (Class a) <- infer m e l
       Bool <- infer m (Map.insert v (Class a) e) r
       return Bool
infer m e (Exists l v r) =
    do Set (Class a) <- infer m e l
       Bool <- infer m (Map.insert v (Class a) e) r
       return Bool
infer m env (IsEmpty e) =
    do Set _ <- infer m env e
       return Bool
infer m env Top = return Bool
infer m env Bot = return Bool
infer m env (Not e) =
    do Bool <- infer m env e
       return Bool
infer m env (And l r) =
    do Bool <- infer m env l
       Bool <- infer m env r
       return Bool
infer m env (Or l r) =
    do Bool <- infer m env l
       Bool <- infer m env r
       return Bool
infer m env (Implies l r) =
    do Bool <- infer m env l
       Bool <- infer m env r
       return Bool
infer m env (Ifte c l r) =
    do Bool <- infer m env c
       Bool <- infer m env l
       Bool <- infer m env r
       return Bool
infer m env (Comp EQ l r) =
    do Int <- infer m env l
       Int <- infer m env r
       return Bool
    `mplus`
    do Class a <- infer m env l
       Class b <- infer m env r
       guard (a == b)
       return Bool
infer m env (Comp _ l r) =
    do Int <- infer m env l
       Int <- infer m env r
       return Bool
infer m env (Size e) =
    do Set _ <- infer m env e
       return Int
infer m env (Sum e) =
    do Bag Int <- infer m env e
       return Int
infer m env (Plus l r) =
    do Int <- infer m env l
       Int <- infer m env r
       return Int
infer m env (Minus l r) =
    do Int <- infer m env l
       Int <- infer m env r
       return Int
infer m e (Id n) = Map.lookup n e
infer m e (Num i) = return Int
infer m e (Const _ c) = return $ Class c
infer m e (Union l r) =
    do Set a <- infer m e l
       Set b <- infer m e r
       guard (a==b)
       return $ Set a
infer m e (AllInstances c) = return $ Set (Class c)
infer m e (Select l x r) =
    do Set (Class a) <- infer m e l
       Bool <- infer m (Map.insert x (Class a) e) r
       return $ Set (Class a)
infer m e (Collect l x r) =
    do Set (Class a) <- infer m e l
       b <- infer m (Map.insert x (Class a) e) r
       return $ Bag b
infer m e (Closure l x r) =
    do Set (Class a) <- infer m e l
       Set (Class b) <- infer m (Map.insert x (Class a) e) r
       guard (a==b)
       return $ Set (Class a)
    `mplus`
    do Class a <- infer m e l
       Set (Class b) <- infer m (Map.insert x (Class a) e) r
       guard (a==b)
       return $ Set (Class a)
-- This will not work properly with boolean
infer m e (Navigate _ s r l) =
    do Class a <- infer m e s
       as <- sequence $ Prelude.map (infer m e) l
       Prop b bs (Class c) <- Map.lookup r e
       guard (length as == length bs)
       guard (all isClass as)
       guard (subtype m a b)
       guard (all (uncurry (subtype m)) (zip (Prelude.map (\(Class x) -> x) as) bs))
       return $ Set (Class c)
    `mplus`
    do Class a <- infer m e s
       as <- sequence $ Prelude.map (infer m e) l
       Prop b bs Bool <- Map.lookup r e
       guard (length as == length bs)
       guard (all isClass as)
       guard (subtype m a b)
       guard (all (uncurry (subtype m)) (zip (Prelude.map (\(Class x) -> x) as) bs))
       return $ Bool
    where isClass (Class _) = True
          isClass _ = False
infer m e (Call _ s r l) =
    do Class a <- infer m e s
       as <- sequence $ Prelude.map (infer m e) l
       Prop b bs (Class c) <- Map.lookup r e
       guard (length as == length bs)
       guard (all isClass as)
       guard (subtype m a b)
       guard (all (uncurry (subtype m)) (zip (Prelude.map (\(Class x) -> x) as) bs))
       return $ Set (Class c)
    where isClass (Class _) = True
          isClass _ = False
infer m g (OclIsKindOf e c) =
    do e' <- infer m g e
       return Bool
infer m g (OclAsType (Id x) c) =
    do Class _ <- Map.lookup x g
       return (Class c)
infer _ _ e = error $ show "Infer missing case: " ++ show e

fixspec :: CD.Package -> OCL.Package -> Maybe OCL.Package
fixspec m (Package p l) = 
    do l' <- sequence $ Prelude.map aux l
       return $ Package p l'
    where aux (InvDecl s e) = 
              do e' <- rectify (initmeta m) (Map.insert "self" (Class s) (initenv m)) e
                 return $ InvDecl s e'
          aux (QueryDecl s n p t e) = 
              do e' <- rectify (initmeta m) (foldr (\(Decl x y) m -> Map.insert x (Class y) m) (Map.insert "self" (Class s) (initenv m)) p) e
                 return $ QueryDecl s n p t e'
          aux (OpDecl s n p pre pos) = 
              do let g' = foldr (\(Decl x y) m -> Map.insert x (Class y) m) (Map.insert "self" (Class s) (initenv m)) p
                 pre' <- sequence $ Prelude.map (rectify (initmeta m) g') pre
                 pos' <- sequence $ Prelude.map (rectify (initmeta m) g') pos
                 return $ OpDecl s n p pre' pos'

rectify :: Meta -> Env -> Expr -> Maybe Expr
rectify m g (Includes (Navigate b (Id s) r l) (Id t)) =
    do guard (all isid l)
       Prop s' l' (Class t') <- Map.lookup r g
       let aux = zip ((s:Prelude.map (\(Id x) -> x) l)++[t]) ((s':l')++[t'])
       e <- sequence $ Prelude.map f aux
       let (cs,vs) = unzip e
       return (And (foldr1 And cs) (Includes (Navigate b (head vs) r (tail (init vs))) (last vs)))
    where isid (Id x) = True
          isid _ = False
          f (v,t) = do Class t' <- Map.lookup v g
                       if subtype m t' t 
                         then return $ (Top, Id v)
                         else return $ (OclIsKindOf (Id v) t, OclAsType (Id v) t)
rectify m g (Select l x r) = 
    do Set (Class c) <- infer m g l
       l' <- rectify m g l
       r' <- rectify m (Map.insert x (Class c) g) r
       return $  Select l' x r'
rectify m g (Collect l x r) = 
    do Set (Class c) <- infer m g l
       l' <- rectify m g l
       r' <- rectify m (Map.insert x (Class c) g) r
       return $ Collect l' x r'
rectify m g (ForAll l x r) = 
    do Set (Class c) <- infer m g l
       l' <- rectify m g l
       r' <- rectify m (Map.insert x (Class c) g) r
       return $  ForAll l' x r'
rectify m g (Exists l x r) = 
    do Set (Class c) <- infer m g l
       l' <- rectify m g l
       r' <- rectify m (Map.insert x (Class c) g) r
       return $  Exists l' x r'
rectify m g (Closure e x r) = 
    do Class c <- infer m g e
       r' <- rectify m (Map.insert x (Class c) g) r
       Set (Class a) <- infer m (Map.insert x (Class c) g) r'
       guard (subtype m a c)
       return $ Closure e x r'
    `mplus`
    do Class c <- infer m g e
       Set (Class a) <- infer m (Map.insert x (Class c) g) r
       guard $ not (subtype m a c)          
       r' <- rectify m (Map.insert x (Class a) g) r
       return $ Closure (OclAsType e a) x r'
rectify m g e = gmapM (mkM (rectify m g)) e