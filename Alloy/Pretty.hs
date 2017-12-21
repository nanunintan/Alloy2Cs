module Alloy.Pretty (pretty) where

import Prelude hiding (LT,GT)
import Text.PrettyPrint.HughesPJ
import Alloy.AST

iden = 3

nl :: Doc
nl = text "\n"

sepComma :: [Doc] -> Doc
sepComma = hcat . (punctuate comma)

angles :: Doc -> Doc
angles d = text "<" <> d <> text ">"

mText :: Maybe String -> Doc
mText Nothing = empty
mText (Just s) = text s

bText :: String -> Bool -> Doc
bText s False = empty
bText s True = text s

mmap :: (a -> b) -> Maybe [a] -> [b]
mmap f Nothing = []
mmap f (Just l) = map f l

may :: Maybe a -> (a -> Doc) -> Doc
may e f = maybe empty f e

pretty :: Module -> String
pretty = render . spec
            
spec :: Module -> Doc
spec (Module Nothing os ps) = vcat (map open os) $$ vcat (punctuate nl (map para ps))
spec (Module (Just m) os ps) = text "module" <+> text m $$ vcat (map open os) $$ vcat (punctuate nl (map para ps))

open :: Open -> Doc
open (Open priv name targs as) =
    bText "private" priv <+> text "open" <+> text name <+>
    brackets (sepComma $ mmap text targs) <+> text "as" <+> mText as

para :: Paragraph -> Doc        
para (FactDecl name (Block e _)) = text "fact" <+> mText name <+> expr False (Block e Nothing)
para (FactDecl name e) = text "fact" <+> mText name <+> expr False (Block [e] Nothing)
para (AssertDecl name e) = text "assert" <+> mText name <+> expr False e
para (FunDecl priv clas name args ret body) =
    text "fun" <+> bText "private" priv <> space <+> may clas (\x -> text (x ++ ".")) <>
    text name <+> may args (brackets . sepComma . (map decl)) <+> text ":" <+>
    expr False ret <+> expr False (Block [body] Nothing)
para (PredDecl priv clas name args body) =
    text "pred" <+> bText "private" priv <> space <+> may clas (\x -> text (x ++ ".")) <>
    text name <+> may args (brackets . sepComma . (map decl)) <+> expr False body
para (SigDecl qualifiers names s fields facts) =
    sep (map qualifier qualifiers) <+> text "sig" <+> sepComma (map text names) <+>
    super s <+> braces (sepComma (map decl fields)) <+>
    may facts ( braces . expr False )
para (EnumDecl s qs) =
    text "enum" <+> text s <+> braces (sepComma (map text qs))
para (RunDecl name e p) = may name folByColon <+> text "run" <+> expr False e <+> scope p
para (CheckDecl name e p) = may name folByColon <+> text "check" <+> expr False e <+> scope p

super :: Super -> Doc
super TopLevel = empty
super (Extends n) = text "extends" <+> text n
super (Inside n) = text "in" <+> text n

scope :: Scope -> Doc
scope (Scope i p expect) = text "for" <+> may i integer <+>
                           case p of
                             [] -> text ""
                             _ -> text "but" <+> sepComma (map typescope p)
                           <+> may expect (precByInt "expect")

typescope :: TypeScope -> Doc
typescope (TypeScope b i n) = integer i <+> text n


folByColon n = text n <> text ":"
precBy p n = text p <+> text n
precByInt p n = text p <+> integer n

                           
qualifier :: SigQual -> Doc
qualifier Abstract = text "abstract"
qualifier SLone = text "lone"
qualifier SOne = text "one"
qualifier SSome = text "some"
qualifier Private = text "private"

decl :: Decl -> Doc
decl (Decl priv disj names disj2 t) = bText "private" priv <+> bText "disj" disj <+>
                                      sepComma (map text names) <+> text ":" <+>
                                      bText "disj" disj2 <+> expr False t

letdecl :: LetDecl -> Doc
letdecl (LetDecl n e) = text n <+> text "=" <+> expr False e

mult :: Mult -> Doc
mult MSome = text "some"
mult MOne = text "one"
mult MLone = text "lone"
mult MSet = text "set"

expr :: Bool -> Expr -> Doc
expr b (Id i _) = text i
expr b (Num i _) = integer i
expr b (None _) = text "none"
expr b (Univ _) = text "univ"
expr b (Iden _) = text "iden"
expr b (Converse e _) = unOp b "~" e
expr b (TransClosure e _) = unOp b "^" e
expr b (RTransClosure e _) = unOp b "^" e
expr b (Cardinality e _) = unOp b "#" e
expr b (Join e1 j2@(Join e2 e3 _) _) = expr True e1 <> text "." <> parens (expr False j2)
expr b (Join e1 e2 _) = expr True e1 <> text "." <> expr True e2
expr b (Intersection e1 e2 _) = binOp b "&" e1 e2
expr b (Arrow e1 m1 m2 e2 _) = paren b $ expr True e1 <+> may m1 mult <+> text "->" <+> may m2 mult <+> expr True e2
expr b (Plus e1 e2 _) = binOp b "+" e1 e2
expr b (Minus e1 e2 _) = binOp b "-" e1 e2
expr b (Override e1 e2 _) = binOp b "++" e1 e2
expr b (DomRestriction e1 e2 _) = binOp b "<:" e1 e2
expr b (RanRestriction e1 e2 _) = binOp b "<:" e1 e2
expr b (BoxJoin e es _) = expr True e <> brackets (hcat $ punctuate comma (map (expr False) es))
expr b (No e _) = unOp b "no " e
expr b (Some e _) = unOp b "some " e
expr b (Lone e _) = unOp b "lone " e
expr b (One e _) = unOp b "one " e
expr b (Set e _) = unOp b "set " e
expr b (Quant q ds e _) = quantifier b q ds e
expr b (In e1 e2 _) = binOp b "in" e1 e2
expr b (Equals e1 e2 _) = binOp b "=" e1 e2
expr b (LT e1 e2 _) = binOp b "<" e1 e2
expr b (LTE e1 e2 _) = binOp b "<=" e1 e2
expr b (GT e1 e2 _) = binOp b ">" e1 e2
expr b (GTE e1 e2 _) = binOp b ">=" e1 e2
expr b (Not (In e1 e2 _) _) = binOp b "not in" e1 e2
expr b (Not (Equals e1 e2 _) _) = binOp b "!=" e1 e2
expr b (Not e1 _) = unOp b "!" e1
expr b (And e1 e2 _) = binOp b "&&" e1 e2
expr b (Implies e1 e2 _) = binOp b "=>" e1 e2
expr b (Iff e1 e2 _) = binOp b "<=>" e1 e2
expr b (Or e1 e2 _) = binOp b "||" e1 e2
expr b (Ifte e1 e2 e3 _) = paren b $ expr True e1 <+> text "=>" <+> expr True e2 <+> text "," <+> expr True e3
expr b (Let es c _) = text "let" <+> sepComma (map letdecl es) <+> text "|" <+> expr b c
expr b (Comprehension ds e _) = braces $ sepComma (map decl ds) <+> text "|" <+> expr False e
expr b (Block es _) = braces $ nest iden (vcat (map (expr False) es)) 
quant :: Quant -> Doc
quant QNo = text "no"
quant QLone = text "lone"
quant QSome = text "some"
quant QOne = text "one"
quant QAll = text "all"
quant QSum = text "sum"

binOp b s e1 e2 = paren b $ expr True e1 <+> text s <+> expr True e2
unOp b s e = text s <> expr True e
quantifier b s es body = paren b $ quant s <+> sepComma (map decl es) <+> text "|" <+> nest iden (expr False body)

paren True = parens
paren False = id