{-# LANGUAGE FlexibleContexts #-}

module Transform.OCL2Alloy (ocl2alloy) where

import Control.Monad.State
import Control.Monad.Error
import Prelude hiding (EQ)
import Data.Generics
import Data.Set hiding (foldr)

import OCL.AST as OCL
import Alloy.AST as Alloy
import CD.AST as CD
import Transform.CD2Alloy

data S = S {
      mutable :: Set OCL.Name,
      self :: String,
      post :: Bool,
      seed :: Int
    }

ocl2alloy :: MonadError String m => CD.Package -> OCL.Package -> m Alloy.Module
ocl2alloy cd ocl = do let m = everything (++) (mkQ [] r) cd
                      let init = S {mutable = fromList m, self = "self", post = False, seed = 0}
                      struct <- cd2alloy cd
                      alloy <- evalStateT (package ocl) init
                      return $ merge struct alloy
    where r (Property n False _ _ _ _) = [n]
          r _ = []

merge (Alloy.Module (Just m) [] p) (Alloy.Module (Just _) [] q) = Alloy.Module (Just m) [] (Prelude.filter struct p ++ q)
        where struct (SigDecl _ _ _ _ _) = True
              struct (EnumDecl _ _) = True
              struct _ = False

package :: MonadError String m => OCL.Package -> StateT S m Alloy.Module
package (OCL.Package m cs) = 
    do cs' <- sequence $ Prelude.map constraint cs
       return $ Alloy.Module (Just m) [] cs'

constraint :: MonadError String m => OCL.Constraint -> StateT S m Alloy.Paragraph
constraint (InvDecl c e) =
    do let x = "self"
       modify (\s -> s {post = False})
       e' <- form e
       return $ FactDecl Nothing (Quant QAll [Alloy.Decl False False ["t"] False (Alloy.Id "Time" Nothing)] 
                                            (Quant QAll [Alloy.Decl False False [x] False (Alloy.Id c Nothing)] e' Nothing) Nothing)
constraint (QueryDecl c f ps r e) =
    do let x = "self"
       modify (\s -> s {post = False})
       let x' = Alloy.Decl False False [x] False (Alloy.Id c Nothing)
       let t = Alloy.Decl False False ["t"] False (Alloy.Id "Time" Nothing)
       let ps' = x' : Prelude.map (\(OCL.Decl v t) -> Alloy.Decl False False [v] False (Alloy.Id t Nothing)) ps ++ [t]
       e' <- expr e
       return $ FunDecl False Nothing f (Just ps') (Set (Alloy.Id r Nothing) Nothing) e'
constraint (OpDecl c o ps pre pos) =
    do let x = "self"
       modify (\s -> s {post = False})
       let x' = Alloy.Decl False False [x] False (Alloy.Id c Nothing)
       let t = Alloy.Decl False False ["t","t'"] False (Alloy.Id "Time" Nothing)
       let ps' = x' : Prelude.map (\(OCL.Decl v t) -> Alloy.Decl False False [v] False (Alloy.Id t Nothing)) ps ++ [t]
       pre' <- sequence $ Prelude.map form pre
       modify (\s -> s {post = True})
       pos' <- sequence $ Prelude.map form pos
       return $ PredDecl False Nothing o (Just ps') (Block (pre' ++ pos') Nothing)

form :: MonadError String m => OCL.Expr -> StateT S m Alloy.Expr
form Top = return $ No (None Nothing) Nothing
form Bot = return $ Some (None Nothing) Nothing
form (OCL.Not l) =
    do l' <- form l
       return $ Alloy.Not l' Nothing
form (OCL.And l r) =
    do l' <- form l
       r' <- form r
       return $ Alloy.And l' r' Nothing
form (OCL.Or l r) =
    do l' <- form l
       r' <- form r
       return $ Alloy.Or l' r' Nothing
form (OCL.Implies l r) =
    do l' <- form l
       r' <- form r
       return $ Alloy.Implies l' r' Nothing
form (Comp EQ l r) =
    do l' <- expr l
       r' <- expr r
       return $ Equals l' r' Nothing
    `catchError`
    \_ -> do l' <- iexpr l
             r' <- iexpr r
             return $ Equals l' r' Nothing
form (Comp OCL.LT l r) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Alloy.LT l' r' Nothing
form (Comp OCL.GT l r) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Alloy.GT l' r' Nothing
form (Comp OCL.LTE l r) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Alloy.LTE l' r' Nothing
form (Comp OCL.GTE l r) =
    do l' <- iexpr l
       r' <- iexpr r
       return $ Alloy.GTE l' r' Nothing
form (ForAll l x r) =
    do l' <- expr l
       r' <- form r
       return $ Quant QAll [Alloy.Decl False False [x] False l'] r' Nothing 
form (Exists l x r) =
    do l' <- expr l
       r' <- form r
       return $ Quant QSome [Alloy.Decl False False [x] False l'] r' Nothing
form (IsEmpty l) =
    do l' <- expr l
       return $ No l' Nothing
form (Includes l r) =
    do l' <- expr l
       r' <- expr r
       return $ In r' l' Nothing
form (Navigate c o r []) =
    do m <- gets mutable
       p <- gets post
       o' <- expr o
       let t = if p && not c then "t'" else "t"
       if r `member` m
       then return $ In o' (Join (Alloy.Id r Nothing) (Alloy.Id t Nothing) Nothing) Nothing
       else return $ In o' (Alloy.Id r Nothing) Nothing
form (OclIsKindOf o a) =
    do o' <- expr o
       return $ In o' (Alloy.Id a Nothing) Nothing
form f = throwError $ "Unsupported OCL formula: " ++ show f

expr :: MonadError String m => OCL.Expr -> StateT S m Alloy.Expr
expr (OCL.Id "self") = 
    do x <- gets self
       return $ Alloy.Id x Nothing
expr (OCL.Id x) = return $ Alloy.Id x Nothing
expr (OCL.Const c x) = return $ Alloy.Id x Nothing
expr (OclAsType o c) = expr o
expr (Navigate c o r l) = 
    do m <- gets mutable
       p <- gets post
       o' <- expr o
       l' <- sequence $ Prelude.map expr l
       let t = if p && not c then "t'" else "t"
       if r `member` m
       then return $ foldr aux (Join o' (Join (Alloy.Id r Nothing) (Alloy.Id t Nothing) Nothing) Nothing) (reverse l')
       else return $ foldr aux (Join o' (Alloy.Id r Nothing) Nothing) (reverse l')
    where aux x y = Join x y Nothing
expr (OCL.Union l r) =
    do l' <- expr l
       r' <- expr r
       return $ Alloy.Plus l' r' Nothing
expr (OCL.Intersection l r) =
    do l' <- expr l
       r' <- expr r
       return $ Alloy.Intersection l' r' Nothing
expr (Select l x r) =
    do l' <- expr l
       r' <- form r
       return $ Comprehension [Alloy.Decl False False [x] False l'] r' Nothing
expr (Reject l x r) =
    do l' <- expr l
       r' <- form r
       return $ Comprehension [Alloy.Decl False False [x] False l'] (Alloy.Not r' Nothing) Nothing
expr (Closure l x r) = 
    do l' <- expr l
       r' <- expr r
       t <- form Top
       y <- fresh
       return $ Join l' (TransClosure (Comprehension [Alloy.Decl False False [x] False (Univ Nothing), Alloy.Decl False False [y] False r'] t Nothing) Nothing) Nothing
expr (AsSet l) = expr l
expr (Call c o r l) = 
    do o' <- expr o
       l' <- sequence $ Prelude.map expr l
       p <- gets post
       let t = if p && not c then "t'" else "t"
       return $ BoxJoin (Alloy.Id r Nothing) (o':l'++[Alloy.Id t Nothing]) Nothing
    where aux x y = Join x y Nothing
expr (AllInstances c) = return $ Alloy.Id c Nothing

expr e = throwError $ "Unsupported OCL set or object expression: " ++ show e

iexpr :: MonadError String m => OCL.Expr -> StateT S m Alloy.Expr
iexpr (OCL.Num n) = return $ Alloy.Num n Nothing
iexpr (OCL.Plus n m) = 
    do n' <- iexpr n
       m' <- iexpr m
       return $ Alloy.Plus n' m' Nothing
iexpr (OCL.Minus n m) = 
    do n' <- iexpr n
       m' <- iexpr m
       return $ Alloy.Minus n' m' Nothing
iexpr (Size e) =
    do e' <- expr e
       return $ Cardinality e' Nothing
iexpr (OCL.Sum (OCL.Collect l x r)) =
    do l' <- expr l
       r' <- iexpr r
       return $ Quant QSum [Alloy.Decl False False [x] False l'] r' Nothing
iexpr f = throwError $ "Unsupported OCL integer expression: " ++ show f


fresh :: MonadError String m => StateT S m String
fresh = do i <- gets seed
           modify (\s -> s {seed = i+1})
           return $ "v" ++ show i

