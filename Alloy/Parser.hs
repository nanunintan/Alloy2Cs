module Alloy.Parser (parseString, parseFile, expr) where

import Prelude hiding (LT, GT, not)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.Char
import Data.List (intersperse)

import Alloy.AST


--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

alloyStyle :: LanguageDef st
alloyStyle = LanguageDef
             { commentStart    = "/*"
             , commentEnd      = "*/"
             , commentLine     = "--"
             , nestedComments  = True
             , identStart      = letter <|> char '_'
             , identLetter     = alphaNum <|> oneOf "_'"
             , opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
             , opLetter        = oneOf ":&+<=>|"
             , reservedNames   = ["sig", "extends", "one", "no", "some", "abstract", "lone", "set", "seq", "private", "let"
                                 ,"private", "disj", "all", "open", "module", "as", "assert", "fact", "check", "run"
                                 ,"for", "expect", "exactly", "fun", "pred", "run", "check", "else", "and", "or", "implies", "in"
                                 , "univ", "none", "iden","enum"]
             , reservedOpNames = []
             , caseSensitive   = True
             }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser alloyStyle

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
integer    = P.integer lexer
parens     = P.parens lexer
angles     = P.angles lexer
brackets   = P.brackets lexer
braces     = P.braces lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

--------------------------------------------------------------------------------
-- Parsing functions
--------------------------------------------------------------------------------

parseString :: String -> Module
parseString s = case parse parser "" s of
                  Left err -> error $ "parse error " ++ (show err)
                  Right val -> val

parseFile :: String -> IO Module
parseFile file = do p <- parseFromFile parser file
                    case p of
                      Left err -> error $ "parse error " ++ (show err)
                      Right val -> return val
       

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
                 
parser :: Parser Module
parser = do whiteSpace; v <- spec; eof; return v

spec :: Parser Module
spec = do reserved "module"
          mod <- optionMaybe identifier
          opens <- many open
          paras <- many paragraph
          return $ Module mod opens paras

open :: Parser Open
open = do priv <- option False (do reserved "private"; return True)
          reserved "open"
          name <- sepBy1 identifier (reservedOp "/")
          params <- optionMaybe (brackets $ sepBy1 identifier (reservedOp ","))
          as <- optionMaybe (do reserved "as"; identifier)
          return $ Open priv (concat $ intersperse "/" name) params as

paragraph :: Parser Paragraph
paragraph = factDecl <|> predDecl <|> assertDecl <|> cmdDecl <|> funDecl <|> sigDecl <|> enumDecl <?> "paragraph"

factDecl :: Parser Paragraph
factDecl = do reserved "fact"; name <- optionMaybe identifier; b <- block; return $ FactDecl name b

assertDecl :: Parser Paragraph
assertDecl = do reserved "assert"; name <- optionMaybe identifier; b <- block; return $ AssertDecl name b

cmdDecl :: Parser Paragraph
cmdDecl = do name <- optionMaybe (do n <-identifier; reservedOp ":"; return n)
             cmd <- choice [(do reserved "run"; return RunDecl), (do reserved "check"; return CheckDecl)]
             body <- choice [do e <- identifier; return $ Id e Nothing, block]
             s <- scope
             return $ cmd name body s
    where scope = do optional (reserved "for")
                     i <- optionMaybe integer
                     optional (reserved "but")
                     scope <- sepBy typeScope (reservedOp ",")
                     expect <- optionMaybe (do reserved "expect"; integer)
                     return $ Scope i scope expect
          typeScope = do exactly <- option False (do reserved "exactly"; return True)
                         i <- integer
                         sig <- identifier
                         return $ TypeScope exactly i sig
           
predDecl :: Parser Paragraph
predDecl = do priv <- option False (do reserved "private"; return True)
              reserved "pred"
              clas <- optionMaybe $ try (do e <- identifier; reservedOp "."; return e)
              name <- identifier
              args <- optionMaybe $ brackets (sepBy decl (reservedOp ","))
              b <- block
              return $ PredDecl priv clas name args b

funDecl :: Parser Paragraph
funDecl = do priv <- option False (do reserved "private"; return True)
             reserved "fun"
             clas <- optionMaybe $ try (do e <- identifier; reservedOp "."; return e)
             name <- identifier
             args <- optionMaybe $ brackets (sepBy decl (reservedOp ","))
             reservedOp ":"
             rtype <- expr
             b <- braces $ expr
             return $ FunDecl priv clas name args rtype b

qualifier :: Parser SigQual
qualifier = choice ( map f [("one", SOne), ("abstract", Abstract), ("lone", SLone), ("some", SSome), ("private", Private)] ) <?> "signature qualifier"
      where f (a, b) = do reserved a; return b
                  
super :: Parser Super
super = do reserved "extends"; n <- identifier; return $ Extends n
        <|>
        do reserved "in"; n <- identifier; return $ Inside n
        <|>
        return TopLevel

sigDecl :: Parser Paragraph
sigDecl = do q <- many qualifier
             reserved "sig"
             names <- sepBy1 identifier (reservedOp ",")
             s <- super
             fields <- braces $ sepBy decl (reservedOp ",")
             fact <- optionMaybe block
             return $ SigDecl q names s fields fact

enumDecl :: Parser Paragraph
enumDecl = do reserved "enum"
              name <- identifier
              consts <- braces $ sepBy1 identifier (reservedOp ",")
              return $ EnumDecl name consts

decl :: Parser Decl
decl = do priv <- option False (f "private")
          disj <- option False (f "disj")
          names <- sepBy1 identifier (reservedOp ",") <?> "variable name"
          reservedOp ":"
          sndDisj <- option False (f "disj")
          t <- expr
          return $ Decl priv disj names sndDisj t
    where f s = do reserved s; return True

letDecl = do name <- identifier
             reservedOp "="
             e <- expr
             return $ LetDecl name e

block = do v <- braces (many expr); return $ Block v Nothing
blockOrBar = block <|> (reservedOp "|" >> expr)

mult :: Parser Mult             
mult = choice $ map (\(o,c) -> do reserved o; return c) [("set",MSet),("one",MOne),("some",MSome),("lone",MLone)]

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- Precedence climbing parsing

expr :: Parser Expr
expr = e 0
    where e 20 = choice uops
          e n  = do l <- e 20; bin l
              where bin l = option l $ do r <- choice $ bops n; bin (r l)
          bops n = map snd $ filter ((>=n).fst) $ [ ( 2, do (reserved "or"  <|> reservedOp "||");  r <- e 3; return $ \l -> Or l r Nothing)
                                                  , ( 2, do (reserved "iff" <|> reservedOp "<=>"); r <- e 4; return $ \l -> Iff l r Nothing)
                                                  , ( 4, do (reserved "implies" <|> reservedOp "=>");  a <- e 4; 
                                                            b <- optionMaybe (do reserved "else"; e 4); 
                                                            case b of Nothing -> return $ \l -> Implies l a Nothing
                                                                      Just b  -> return $ \l -> Ifte l a b Nothing)
                                                  , ( 5, do (reserved "and" <|> reservedOp "&&"); r <- e 6; return $ \l -> And l r Nothing)
                                                  , ( 7, do c <- try (negated "in"); r <- e 8; return $ \l -> c $ In l r Nothing)
                                                  , ( 7, do c <- try (negated "=");  r <- e 8; return $ \l -> c $ Equals l r Nothing)
                                                  , ( 7, do c <- try (negated "<");  r <- e 8; return $ \l -> c $ LT l r Nothing)
                                                  , ( 7, do c <- try (negated ">");  r <- e 8; return $ \l -> c $ GT l r Nothing)
                                                  , ( 7, do c <- try (negated "<="); r <- e 8; return $ \l -> c $ LTE l r Nothing)
                                                  , ( 7, do c <- try (negated ">="); r <- e 8; return $ \l -> c $ GTE l r Nothing)
                                                  , (10, do reservedOp "+";  r <- e 11; return $ \l -> Plus l r Nothing)
                                                  , (10, do reservedOp "-";  r <- e 11; return $ \l -> Minus l r Nothing)
                                                  , (12, do reservedOp "++"; r <- e 13; return $ \l -> Override l r Nothing)
                                                  , (13, do reservedOp "&";  r <- e 14; return $ \l -> Intersection l r Nothing)
                                                  , (14, do m <- try (do m <- optionMaybe mult; reservedOp "->"; return m); 
                                                            n <- optionMaybe mult; r <- e 14; return $ \l -> Arrow l m n r Nothing)
                                                  , (15, do reservedOp "<:"; r <- e 16; return $ \l -> DomRestriction l r Nothing)
                                                  , (16, do reservedOp ":>"; r <- e 17; return $ \l -> RanRestriction l r Nothing)
                                                  , (17, do r <- brackets (sepBy expr (reservedOp ",")); return $ \l -> BoxJoin l r Nothing) 
                                                  , (18, do reservedOp "."; r <- e 19; return $ \l -> Join l r Nothing) ]
          uops = [ do reserved "let"; d <- sepBy1 letDecl (reservedOp ","); e <- blockOrBar; return $ Let d e Nothing
                 , try $ do reserved "all";  d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QAll d y Nothing
                 , try $ do reserved "some"; d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QSome d y Nothing
                 , try $ do reserved "lone"; d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QLone d y Nothing
                 , try $ do reserved "one";  d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QOne d y Nothing
                 , try $ do reserved "no";   d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QNo d y Nothing
                 , try $ do reserved "sum";  d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Quant QSum d y Nothing
                 , braces $ do d <- sepBy1 decl (reservedOp ","); y <- blockOrBar; return $ Comprehension d y Nothing
                 , do (reservedOp "!" <|> reserved "not"); r <- e  6; return $ Not r Nothing
                 , do reservedOp "#"; r <- e 11; return $ Cardinality r Nothing
                 , do reservedOp "~"; r <- e 19; return $ Converse r Nothing
                 , do reservedOp "^"; r <- e 19; return $ TransClosure r Nothing
                 , do reservedOp "*"; r <- e 19; return $ RTransClosure r Nothing
                 , do reserved "no";   r <- e 8; return $ No r Nothing
                 , do reserved "some"; r <- e 8; return $ Some r Nothing
                 , do reserved "one";  r <- e 8; return $ One r Nothing
                 , do reserved "lone"; r <- e 8; return $ Lone r Nothing
                 , do reserved "set";  r <- e 8; return $ Set r Nothing
                 , do n <- identifier; return $ Id n Nothing
                 , do n <- integer; return $ Num n Nothing
                 , do reserved "none"; return $ None Nothing
                 , do reserved "iden"; return $ Iden Nothing
                 , do reserved "univ"; return $ Univ Nothing
                 , parens expr ]
-- does not work well if there is not a space after !
          negated s = do b <- option False ((reserved "not" <|> reserved "!") >> return True);
                         reservedOp s
                         if b then return $ \e -> Not e Nothing else return id
