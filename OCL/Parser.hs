module OCL.Parser (parseString, parseFile) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Prelude hiding (GT,LT,EQ)

import OCL.AST


--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

oclStyle :: LanguageDef st
oclStyle = LanguageDef
             { commentStart    = "/*"
             , commentEnd      = "*/"
             , commentLine     = "--"
             , nestedComments  = True
             , identStart      = letter <|> char '_'
             , identLetter     = alphaNum <|> oneOf "_"
             , opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
             , opLetter        = oneOf ":&+<=>|"
             , reservedNames   = ["context","inv","pre","post","and","or","implies","if","then","else","endif","package","endpackage"]
             , reservedOpNames = []
             , caseSensitive   = True
             }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser oclStyle

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

parseString :: String -> Package
parseString s = case parse parser "" s of
                  Left err -> error $ "parse error " ++ (show err)
                  Right val -> val

parseFile :: String -> IO Package
parseFile file = do p <- parseFromFile parser file
                    case p of
                      Left err -> error $ "parse error " ++ (show err)
                      Right val -> return val
       

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
                 
parser :: Parser Package
parser = do whiteSpace; v <- package; eof; return v

package :: Parser Package
package = do reserved "package"
             p <- identifier
             cs <- many constraint
             reserved "endpackage"
             return $ Package p cs

constraint :: Parser Constraint
constraint = try invariant <|> try query <|> operation <?> "constraint"

invariant :: Parser Constraint
invariant = do reserved "context"
               c <- identifier
               reserved "inv"
               reservedOp ":"
               e <- expr
               return $ InvDecl c e

query :: Parser Constraint
query = do reserved "context"
           c <- identifier
           reservedOp "::"
           q <- identifier
           ds <- parens (sepBy decl (reservedOp ","))
           reservedOp ":"
           reserved "Set"
           r <- parens identifier
           reserved "body"
           reservedOp ":"
           e <- expr
           return $ QueryDecl c q ds r e

operation :: Parser Constraint
operation = do reserved "context"
               c <- identifier
               reservedOp "::"
               q <- identifier
               ds <- parens (sepBy decl (reservedOp ","))
               cs <- many aux
               let (pre,post) = divide cs
               return $ OpDecl c q ds pre post
    where aux = do reserved "pre"
                   reservedOp ":"
                   e <- expr
                   return $ Left e
                <|>
                do reserved "post"
                   reservedOp ":"
                   e <- expr
                   return $ Right e
          divide = foldr (\h (l,r) -> case h of Left x -> (x:l,r); Right x -> (l,x:r)) ([],[])

decl :: Parser Decl
decl = do x <- identifier
          reservedOp ":"
          c <- identifier
          return $ Decl x c

expr :: Parser Expr
expr = e 0
    where e 20 = choice uops
          e n  = do l <- e 20; bin l
              where bin l = option l $ do r <- choice $ bops n; bin (r l)
          bops n = map snd $ filter ((>=n).fst) $ [ ( 2, do reserved "implies";  r <- e 3; return $ \l -> Implies l r)
                                                  , ( 3, do reserved "or"; r <- e 4; return $ \l -> Or l r)
                                                  , ( 5, do reserved "and"; r <- e 6; return $ \l -> And l r)
                                                  , ( 6, do reservedOp "="; r <- e 7; return $ \l -> Comp EQ l r)
                                                  , ( 7, do reservedOp "<"; r <- e 8; return $ \l -> Comp LT l r)
                                                  , ( 7, do reservedOp ">"; r <- e 8; return $ \l -> Comp GT l r)
                                                  , ( 7, do reservedOp "<="; r <- e 8; return $ \l -> Comp LTE l r)
                                                  , ( 7, do reservedOp ">="; r <- e 8; return $ \l -> Comp GTE l r)
                                                  , ( 8, do reservedOp "+"; r <- e 9; return $ \l -> Plus l r)
                                                  , ( 8, do reservedOp "-"; r <- e 9; return $ \l -> Minus l r)
                                                  , (11, do reserved "->isEmpty()"; return $ \l -> IsEmpty l)
                                                  , (11, do reserved "->includes"; e <- parens expr; return $ \l -> Includes l e)
                                                  , (11, do reserved "->forAll"; (x,e) <- parens aux; return $ \l -> ForAll l x e)
                                                  , (11, do reserved "->exists"; (x,e) <- parens aux; return $ \l -> Exists l x e)
                                                  , (11, do reserved "->size()"; return $ \l -> Size l)
                                                  , (11, do reserved "->sum()"; return $ \l -> Sum l)
                                                  , (11, do reserved "->asSet()"; return $ \l -> AsSet l)
                                                  , (11, do reserved "->union"; e <- parens expr; return $ \l -> Union l e)
                                                  , (11, do reserved "->intersection"; e <- parens expr; return $ \l -> Intersection l e)
                                                  , (11, do reserved "->select"; (x,e) <- parens aux; return $ \l -> Select l x e)
                                                  , (11, do reserved "->reject"; (x,e) <- parens aux; return $ \l -> Reject l x e)
                                                  , (11, do reserved "->collect"; (x,e) <- parens aux; return $ \l -> Collect l x e)
                                                  , (11, do reserved "->closure"; (x,e) <- parens aux; return $ \l -> Closure l x e)
                                                  , (11, do reserved ".oclAsType"; e <- parens identifier; return $ \l -> OclAsType l e)
                                                  , (11, do reserved ".oclIsKindOf"; e <- parens identifier; return $ \l -> OclIsKindOf l e)
                                                  , (11, try $ do reservedOp "."; 
                                                                  r <- identifier; 
                                                                  b <- option False (do reserved "@pre"; return True)
                                                                  ps <- parens (sepBy expr (reservedOp ",")); 
                                                                  return $ \l -> Call b l r ps)
                                                  , (11, do reservedOp "."; 
                                                            r <- identifier; 
                                                            b <- option False (do reserved "@pre"; return True)
                                                            ps <- option [] $ brackets (sepBy expr (reservedOp ",")); 
                                                            return $ \l -> Navigate b l r ps)
                                                  ]
          uops = [ do reserved "not"; r <- e  10; return $ Not r
                 , do reserved "true"; return Top
                 , do reserved "false"; return Bot
                 , do reserved "if"; c <- expr; reserved "then"; t <- expr; reserved "else"; e <- expr; reserved "endif"; return $ Ifte c t e
                 , try $ do c <- identifier; reservedOp "::"; k <- identifier; return $ Const c k
                 , try $ do c <- identifier; reserved ".allInstances()"; return $ AllInstances c
                 , do n <- identifier; return $ Id n
                 , do n <- integer; return $ Num n
                 , parens expr ]
          aux = do x <- identifier
                   reservedOp "|"
                   e <- expr
                   return (x,e)
