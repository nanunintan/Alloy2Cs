module OCL.Simplification (simplify, replace) where

import Data.Generics
import Data.Maybe
import Prelude hiding (EQ)
import Control.Monad

import OCL.AST

simplify :: Package -> Package
simplify = fromJust . many (onepoint ||| reflex ||| basic ||| quants ||| casts ||| select ||| asset)

reflex :: Rule Package
reflex = somewhere (mkMp t)
    where t (Comp EQ a b) | a == b = return Top
          t _ = mzero

basic :: Rule Package
basic = somewhere (mkMp t)
    where t (And Top e) = return e
          t (And e Top) = return e
          t (And l r) | l == r = return l
          t (And l (And r e)) | l == r = return $ And l e
          t (Or e Bot) = return e
          t (Or Bot e) = return e
          t (Or l r) | l == r = return l
          t (Or l (Or r e)) | l == r = return $ Or l e
          t (Implies Top e) = return e
          t _ = mzero

quants :: Rule Package
quants = somewhere (mkMp t)
    where t (ForAll e x t) = do t' <- erase (Includes e (Id x)) t
                                return $ ForAll e x t'
          t (Exists e x t) = do t' <- erase (Includes e (Id x)) t
                                return $ Exists e x t'
          t (Select e x t) = do t' <- erase (Includes e (Id x)) t
                                return $ Select e x t'
          t _ = mzero

asset :: Rule Package
asset = somewhere (mkMp t)
    where t (AsSet (Closure l x r)) = return $ Closure l x r
          t (Size (AsSet e)) = return $ Size e
          t _ = mzero

select :: Rule Package
select = somewhere (mkMp t)
    where t (Select (AllInstances c) x (Includes e (Id y))) | x==y && x `notElem` free [] e = return $ AsSet e
          t _ = mzero

casts :: Rule Package
casts = somewhere (mkMp t)
    where t (ForAll (AllInstances c) x t) = do t' <- erase (OclIsKindOf (Id x) c) t
                                               return $ ForAll (AllInstances c) x t'
          t (Select (AllInstances c) x t) = do t' <- erase (OclIsKindOf (Id x) c) t
                                               return $ Select (AllInstances c) x t'
          t (Exists (AllInstances c) x t) = do t' <- erase (OclIsKindOf (Id x) c) t
                                               return $ Exists (AllInstances c) x t'
          t _ = mzero

-- This is unsafe when e contains free variables
erase :: Expr -> Rule Expr
erase e = somewhere (mkMp t)
    where t x | x == e = return Top
              | otherwise = mzero

onepoint :: Rule Package
onepoint = somewhere (mkMp t)
    where t (Exists e n c) = do r <- same n c
                                return $ replace n r c
          t (ForAll e n (Implies c t)) = do r <- same n c
                                            return $ replace n r (Implies c t)
          t _ = mzero

same :: Name -> Expr -> Maybe Expr
same n (Comp EQ (Id x) e) | x==n && x `notElem` free [] e = return $ e
same n (Comp EQ e (Id x)) | x==n && x `notElem` free [] e = return $ e
same n (And l r) = same n l `mplus` same n r
same _ _ = Nothing

replace :: Name -> Expr -> Expr -> Expr
replace n e (Id m) | n==m = e
                   | otherwise = Id m
replace n e (Navigate b l s r) = Navigate b (replace n e l) s (map (replace n e) r)
replace n e (Call b l s r) = Call b (replace n e l) s (map (replace n e) r)
replace n e (ForAll l m r) = ForAll (replace n e l) m (if n==m then r else replace n e r)
replace n e (Exists l m r) = Exists (replace n e l) m (if n==m then r else replace n e r)
replace n e (Select l m r) = Select (replace n e l) m (if n==m then r else replace n e r)
replace n e (Collect l m r) = Collect (replace n e l) m (if n==m then r else replace n e r)
replace n e (Closure l m r) = Closure (replace n e l) m (if n==m then r else replace n e r)
replace n e x = gmapT (mkT (replace n e)) x

free :: [Name] -> Expr -> [Name]
free env (Id n) | n `elem` env = []
                | otherwise = [n]
free env (Navigate _ e _ l) = free env e ++ concat (map (free env) l)
free env (ForAll l n r) = free env l ++ free (n:env) r
free env (Exists l n r) = free env l ++ free (n:env) r
free env (Select l n r) = free env l ++ free (n:env) r
free env (Collect l n r) = free env l ++ free (n:env) r
free env (Closure l n r) = free env l ++ free (n:env) r
free env e = gmapQr (++) [] (mkQ [] (free env)) e

type Rule a = a -> Maybe a

nop :: Rule a
nop = return

(|||) :: Rule a -> Rule a -> Rule a
(r ||| s) t = r t `mplus` s t

(>>>) :: Rule a -> Rule a -> Rule a
(r >>> s) t = r t >>= s

many :: Rule a -> Rule a
many r = (r >>> many r) ||| nop
