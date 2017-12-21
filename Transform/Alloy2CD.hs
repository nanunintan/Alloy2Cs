module Transform.Alloy2CD (alloy2cd) where

import Data.Set as Set
import Data.Map as Map
import Control.Monad
import Data.List (nub)
import Data.Maybe (catMaybes)

import Alloy.Types
import Alloy.AST
import Alloy.Parser
import CD.AST as CD
import CD.Pretty

test :: String -> IO ()
test f = do p <- parseFile f
            putStrLn $ pretty $ alloy2cd p

-- This needs to be updated to a partial transformation instead of ignoring unsupported features
alloy2cd :: Module -> Package
alloy2cd p = let m = meta p
             in Package (name m) (classes m ++ enums m)
    where enums m = let cs = Prelude.map (\c -> (parent m ! c,c)) $ Set.toList (consts m)
                        ps = nub $ Prelude.map fst cs
                     in Prelude.map (\c -> Enumeration c (Prelude.map (Literal . snd) $ Prelude.filter (\(x,_) -> x==c) cs)) ps
          classes m = let cs = Set.toList (Set.delete "univ" $ Set.delete "Time" $ Set.difference (Set.difference (sigs m) (consts m)) (Set.map (parent m !) (consts m)))
                       in Prelude.map (\c -> Class c (Set.member c (abstract m)) (general m c ++ atribs m c ++ funcs m c ++ ops m c)) cs
          general m c = case Map.lookup c (parent m) of Just "univ" -> []
                                                        Just p      -> [Generalization p]
                                                        Nothing     -> []
          atribs m c = let rs = rels m
                           as = catMaybes $ Prelude.map reltoatrib rs
                        in Prelude.map snd $ Prelude.filter (\(x,_) -> x==c) as
          funcs m c = let fs = funs m
                          as = catMaybes $ Prelude.map funtoquery fs
                       in Prelude.map snd $ Prelude.filter (\(x,_) -> x==c) as
          ops m c = let fs = preds m
                        as = catMaybes $ Prelude.map predtoop fs
                     in Prelude.map snd $ Prelude.filter (\(x,_) -> x==c) as

reltoatrib :: (String,Expr) -> Maybe (String, Owned)
reltoatrib (m,Id s Nothing) = return $ (s, Property (rename m) True (Just (PrimitiveType "Boolean")) (ehigh MOne) (elow MOne) [])
reltoatrib (n,e) = do l <- names e
                      m <- mult e
                      let (l',r) = if (last l == "Time") then (init l,False) else (l,True)
                      guard $ length l' == 2 && m == MOne
                      return $ (head l', Property (rename n) r (Just (RefType (last l'))) (ehigh m) (elow m) [])
                   `mplus`
                   do l <- names e
                      m <- mult e
                      let (l',r) = if (last l == "Time") then (init l,False) else (l,True)
                      guard $ length l' > 2 || m /= MOne
                      return $ (head l', Property (rename n) r (Just (RefType (last l'))) (ehigh m) (elow m) (Prelude.map (\(x,y) -> Qualified x (RefType y)) $ zip (Prelude.map (:[]) ['a'..]) (tail (init l'))))


funtoquery :: (String,Expr) -> Maybe (String,Owned)
funtoquery (n,e) = do l <- names e
                      let l' = if (last (init l) == "Time") then init (init l) ++ [last l] else l
                      return $ (head l', Operation n $ (Prelude.map (\(x,y) -> InOut x (RefType y)) $ zip (Prelude.map (:[]) ['a'..]) (tail (init l')))++[Return (RefType (last l')) (ehigh MSet) (elow MSet)])

predtoop :: (String,Expr) -> Maybe (String,Owned)
predtoop (n,e) = do l <- names e
                    guard (length l >= 3 && last l == "Time" && last (init l) == "Time")
                    let l' = init (init l)
                    return $ (head l', Operation n (Prelude.map (\(x,y) -> InOut x (RefType y)) $ zip (Prelude.map (:[]) ['a'..]) (tail l')))

names (Id n Nothing) = return $ [n]
names (Arrow l _ _ r Nothing) = do l' <- names l
                                   r' <- names r
                                   return $ l' ++ r'
names _ = mzero

mult (Id _ Nothing) = return MSet
mult (Arrow _ Nothing Nothing (Id "Time" _) _) = return MSet
mult (Arrow _ m Nothing (Id "Time" _) _) = m
mult (Arrow _ Nothing Nothing (Id _ _) _) = return MSet
mult (Arrow _ Nothing m (Id _ _) _) = m
mult (Arrow _ Nothing Nothing e _) = mult e
mult _ = mzero
                   
ehigh MSet = Just (LUnlimitedNatural (-1))
ehigh MSome = Just (LUnlimitedNatural (-1))
ehigh MLone = Just (LUnlimitedNatural 1)
ehigh MOne = Just (LUnlimitedNatural 1)

elow MSet = Just (LInteger 0)
elow MSome = Just (LInteger 1)
elow MLone = Just (LInteger 0)
elow MOne = Just (LInteger 1)

rename s | '.' `elem` s = tail $ dropWhile (/='.') s
         | otherwise = s
