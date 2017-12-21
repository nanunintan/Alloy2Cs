module OCL.Pretty (pretty) where

import Text.PrettyPrint
import Prelude hiding (EQ,GT,LT)
import OCL.AST

iden = 3

pretty :: Package -> String
pretty = render . spec

spec :: Package -> Doc
spec (Package m ps) = text "package" <+> text m $$ nest iden (vcat $ map para ps) $$ text "endpackage"

para :: Constraint -> Doc
para (InvDecl c f) = (text "context" <+> text c) $$ nest iden (text "inv:" <+> expr False f)
para (QueryDecl c n ds r e) = (text "context" <+> text c <> text "::" <> text n <> 
                               parens (hcat (punctuate comma (map decl ds))) <> 
                               colon <> text "Set" <> parens (text r)) $$ nest iden (text "body:" <+> expr False e)
para (OpDecl c n ds pre pos) = (text "context" <+> text c <> text "::" <> text n <> 
                               parens (hcat (punctuate comma (map decl ds)))) 
                               $$ nest iden (vcat (map (\f -> text "pre:" <+> expr False f) pre))
                               $$ nest iden (vcat (map (\f -> text "post:" <+> expr True f) pos))

decl :: Decl -> Doc
decl (Decl n c) = text n <> colon <> text c

expr :: Bool -> Expr -> Doc
expr b (Includes e n) = expr b e <> arrow <> text "includes" <> parens (expr b n)
expr b (ForAll e n f) = expr b e <> arrow <> text "forAll" <> parens (text n <+> bar <+> expr b f)
expr b (Exists e n f) = expr b e <> arrow <> text "exists" <> parens (text n <+> bar <+> expr b f)
expr b (IsEmpty e) = expr b e <> arrow <> text "isEmpty()"
expr b Top = text "true"
expr b Bot = text "false"
expr b (Not f) = unOp "not" (expr b f)
expr b (And l r) = binOp "and" (expr b l) (expr b r)
expr b (Or l r) = binOp "or" (expr b l) (expr b r)
expr b (Implies l r) = binOp "implies" (expr b l) (expr b r)
expr b (Ifte c t e) = parens $ text "if" <+> expr b c <+> text "then" <+> expr b t <+> text "else" <+> expr b e <+> text "endif"
expr b (Comp o l r) = binOp (op o) (expr b l) (expr b r)
expr b (Size e) = expr b e <> arrow <> text "size()"
expr b (Sum e) = expr b e <> arrow <> text "sum()"
expr b (Plus l r) = binOp "+" (expr b l) (expr b r)
expr b (Minus l r) = binOp "-" (expr b l) (expr b r)
expr b (Id n) = text n
expr b (Num n) = text (show n)
expr b (Const n p) = text p <> text "::" <> text n
expr b (Navigate c n a []) = expr b n <> dot <> text a <> (if b && c then text "@pre" else empty)
expr b (Navigate c n a qs) = expr b n <> dot <> text a <> (if b && c then text "@pre" else empty) <> 
                             brackets (hcat $ punctuate comma $ map (expr b) qs)
expr b (Call c n a qs) = expr b n <> dot <> text a <> (if b && c then text "@pre" else empty) <> parens (hcat $ punctuate comma $ map (expr b) qs)
expr b (Union l r) = expr b l <> arrow <> text "union" <> parens (expr b r)
expr b (AllInstances c)    = text c <> dot <> text "allInstances()"
expr b (Select e v f)      =  expr b e <> arrow <> text "select" <> parens (text v <+> bar <+> expr b f)
expr b (Reject e v f)      =  expr b e <> arrow <> text "reject" <> parens (text v <+> bar <+> expr b f)
expr b (Collect e v i)     =  expr b e <> arrow <> text "collect" <> parens (text v <+> bar <+> expr b i)
expr b (Closure e v s)     =  expr b e <> arrow <> text "closure" <> parens (text v <+> bar <+> expr b s)
expr b (OclAsType e c) = expr b e <> text ".oclAsType" <> parens (text c)
expr b (OclIsKindOf e c) = expr b e <> text ".oclIsKindOf" <> parens (text c)
expr b (AsSet e) = expr b e <> text "->asSet()"


op :: Op -> String
op EQ = "="
op GT = ">"
op LT = "<"
op GTE = ">="
op LTE = "<="

binOp s e1 e2 = parens $ e1 <+> text s <+> e2
unOp s e = parens $ text s <+> e

arrow = text "->"
dot = text "."
bar = text "|"