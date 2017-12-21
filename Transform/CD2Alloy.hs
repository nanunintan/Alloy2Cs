{-# LANGUAGE FlexibleContexts #-}

module Transform.CD2Alloy (cd2alloy) where

import Control.Monad.Error
import Data.Generics
import Data.Maybe (catMaybes)

import CD.AST as CD
import CD.Parser
import Alloy.AST as Alloy
import Alloy.Pretty

test :: String -> IO ()
test f = do f <- parseFile f
            case (cd2alloy f) of Left error  -> putStrLn $ "Error: " ++ error
                                 Right alloy -> putStrLn (pretty alloy)

cd2alloy :: MonadError String m => CD.Package -> m Alloy.Module
cd2alloy (Package n l) = 
    do es <- sequence $ map enum l
       cs <- sequence $ map sig l
       return $ Module (Just n) [] $ time:(concat cs ++ catMaybes es)

enum :: MonadError String m => Packaged -> m (Maybe Paragraph)
enum (Enumeration n l) = return $ return $ EnumDecl n (map (\(Literal n) -> n) l)
enum _ = return $ mzero

time :: Paragraph
time = SigDecl [] ["Time"] TopLevel [] Nothing

sig :: MonadError String m => Packaged -> m [Paragraph]
sig (Class n b l) = 
    do let q = if b then [Abstract] else []
       as <- sequence $ map atrib l
       ss <- sequence $ map (subset n) l
       os <- sequence $ map (ops n) l
       unless (length (parent l) <= 1) $ throwError "Signature with more then one generalization"
       let s = case parent l of [] -> TopLevel; [p] -> Extends p
       return $ SigDecl q [n] s (catMaybes as) Nothing : (catMaybes ss ++ catMaybes os)
sig _ = return $ mzero

parent :: [Owned] -> [String]
parent = everything (++) (mkQ [] q)
    where q (Generalization p) = [p]
          q _ = []

atrib :: MonadError String m => Owned -> m (Maybe Decl)
atrib (Property n b (Just (RefType t)) h l []) =
    do m <- mult h l
       if b 
       then return $ return $ Decl False False [n] False $ aux m (Id t Nothing)
       else return $ return $ Decl False False [n] False $ Arrow (Id t Nothing) (Just m) Nothing (Id "Time" Nothing) Nothing
    where aux MSet = \x -> Set x Nothing
          aux MOne = \x -> One x Nothing
          aux MSome = \x -> Some x Nothing
          aux MLone = \x -> Lone x Nothing
atrib (Property n b (Just (RefType t)) h l q) =
    do m <- mult h l
       let q' = map (\(Qualified _ (RefType x)) -> Id x Nothing) q
       if b 
       then return $ return $ Decl False False [n] False $ foldr arrow (Arrow (last q') Nothing (Just m) (Id t Nothing) Nothing) (init q')
       else return $ return $ Decl False False [n] False $ foldr arrow (Arrow (Id t Nothing) (Just m) Nothing (Id "Time" Nothing) Nothing) q'
atrib (Property n False (Just (PrimitiveType "Boolean")) h l []) =
    do MOne <- mult h l
       return $ return $ Decl False False [n] False $ Set (Id "Time" Nothing) Nothing
atrib _ = return $ mzero

subset :: MonadError String m => String -> Owned -> m (Maybe Paragraph)
subset c (Property n True (Just (PrimitiveType "Boolean")) h l []) =
    do MOne <- mult h l
       return $ return $ SigDecl [] [n] (Inside c) [] Nothing
subset _ _ = return $ mzero

ops :: MonadError String m => String -> Owned -> m (Maybe Paragraph)
ops c (Operation n p) =
    case everything (++) (mkQ [] aux) p
    of [Return (RefType r) h l] -> do MSet <- mult h l
                                      p' <- sequence $ map decl p
                                      let s = Decl False False ["self"] False (Id c Nothing)
                                          ts = [Decl False False ["t"] False (Id "Time" Nothing)]
                                      return $ return $ FunDecl False Nothing n (Just (s:concat p'++ts)) (Set (Id r Nothing) Nothing) (None Nothing)
       [] -> do p' <- sequence $ map decl p
                let s = Decl False False ["self"] False (Id c Nothing)
                    ts = [Decl False False ["t","t'"] False (Id "Time" Nothing)]
                return $ return $ PredDecl False Nothing n (Just (s:concat p'++ts)) (Block [] Nothing)
       _ -> throwError "Unsupported operation type"
    where aux (Return r h l) = [Return r h l]
          aux _ = []
ops c _ = return $ mzero

decl (InOut n (RefType t)) = return $ return $ Decl False False [n] False (Id t Nothing)
decl (Return _ _ _) = return $ mzero
decl _ = throwError "Parameters cannot be primitive types"

mult (Just (LUnlimitedNatural (-1))) Nothing = return $ MSet
mult (Just (LUnlimitedNatural (-1))) (Just (LInteger 0)) = return $ MSet
mult (Just (LUnlimitedNatural (-1))) (Just (LInteger 1)) = return $ MSome
mult (Just (LUnlimitedNatural 1)) Nothing = return $ MLone
mult (Just (LUnlimitedNatural 1)) (Just (LInteger 0)) = return $ MLone
mult (Just (LUnlimitedNatural 1)) (Just (LInteger 1)) = return $ MOne
mult _ _ = throwError $ "Unsupported multiplicity"

arrow e q = Arrow e Nothing Nothing q Nothing