module Cs.Pretty (pretty) where

import Text.PrettyPrint
import Prelude hiding (EQ,GT,LT)
import Cs.AST
import Data.Char

pretty :: Namespace -> String
pretty = render . spec

spec :: Namespace -> Doc
spec (Namespace n elems) = usings <> (namespaceHeader n) <+>
                             contextDeclaration <>
                              vcat (punctuate (empty) (map (\x -> dbSet x) elems) ) <>
                              contextConfig n <>
                             contextDeclarationEnd
                             $$ vcat (punctuate (empty) (map (\x -> pack x) elems) ) <> 
                            namespaceEnd 

dbSet :: Packaged  -> Doc
dbSet (Class n abs ows)  = newLine <> doubleHorizontalSpace <> 
                           text "public DbSet<" <> identifier n <> text ">" <+> identifier n <+> text "{ get; set; }"
dbSet (Enumeration n ls) = empty                 

pack :: Packaged  -> Doc
pack (Class n abs ows)  = newLine <> simpleHorizontalSpace <> 
                          abstract abs <> modifier "public" <+> simpleType "class" <+> identifier n <>
                          vcat (punctuate (empty) (map (\x -> parentCl x) ows) ) <>
                          newLine <> simpleHorizontalSpace <> blockBegin <> 
                           propertyId
                           $$ vcat (punctuate (empty) (map (\x -> owCl x) ows) ) <> 
                          newLine <> simpleHorizontalSpace <> blockEnd
pack (Enumeration n ls) = newLine <> simpleHorizontalSpace <> 
                          modifier "public" <+> simpleType "enum" <+> identifier n <> 
                          newLine <> simpleHorizontalSpace <> blockBegin <> 
                           lits ls <> 
                          newLine <> simpleHorizontalSpace <> blockEnd

parentCl :: Owned -> Doc
parentCl (Generalization gl) = text " : " <> text gl
parentCl ow = empty

owCl :: Owned -> Doc
owCl (Property n r ty upp low qs) = doubleHorizontalSpace <>
                           modifier "public" <+> csType (upper upp) (lower low) ty <+> capitalizedIdentifier n <+> readOnly r 
owCl (Operation n ps   ) = doubleHorizontalSpace <>
                           modifier "public" <+> 
                           paramsReturn ps <+> 
                           name n <+> 
                           text "(" <> params ps <> text ")" <> operationBlock ps
owCl ow                  = empty

params :: [Parameter] -> Doc
params []    = empty
params (x:l) = if isInOutParam x == False then params l
  else param x <>
    if thereIsInOutParam l then text ", " <> params l
    else empty

thereIsInOutParam :: [Parameter] -> Bool
thereIsInOutParam [] = False
thereIsInOutParam (x:l) = if isInOutParam x then True
 else thereIsInOutParam l
 
thereIsReturnParam :: [Parameter] -> Bool
thereIsReturnParam [] = False
thereIsReturnParam (x:l) = if isInOutParam x == False then True
 else thereIsReturnParam l

isInOutParam :: Parameter -> Bool
isInOutParam (InOut n t)      = True
isInOutParam p = False
  
param :: Parameter -> Doc
param (InOut n t)      = fTyp t <+> name n
param (Return t up lw) = empty

fTyp :: Type -> Doc
fTyp (RefType idRef) = typeInCS idRef
fTyp (PrimitiveType hRef) = text "object"

paramsReturn :: [Parameter] -> Doc
paramsReturn []    = text "void"
paramsReturn (x:l) = if isInOutParam x == False then opReturn x
  else paramsReturn l

opReturn :: Parameter -> Doc
opReturn (InOut n t)      = empty
opReturn (Return t up lw) = csfType (upper up) (lower lw) t

csfType :: Integer -> Integer -> Type -> Doc 
csfType upp low ty
    | upp == 0              = text "object"  
    | upp == 1  && low == 0 = ftagTyp ty <> if fIsBuiltInTypes ty then text "?" else empty
    | upp == 1  && low == 1 = ftagTyp ty
    | upp == -1             = text "ICollection<" <> ftagTyp ty <> text ">"
    | otherwise             = ftagTyp ty
    
ftagTyp :: Type -> Doc
ftagTyp (RefType idRef) = typeInCS idRef 
ftagTyp (PrimitiveType hRef) = typeInCS hRef

csType :: Integer -> Integer -> (Maybe Type) -> Doc 
csType upp low ty
    | upp == 0              = tagTyp Nothing  
    | upp == 1  && low == 0 = tagTyp ty <> if mIsBuiltInTypes ty then text "?" else empty
    | upp == 1  && low == 1 = tagTyp ty
    | upp == -1             = text "ICollection<" <> tagTyp ty <> text ">"
    | otherwise             = tagTyp ty  

tagTyp :: (Maybe Type) -> Doc
tagTyp Nothing = text "object"
tagTyp (Just (RefType idRef)) = typeInCS idRef 
tagTyp (Just (PrimitiveType hRef)) = typeInCS hRef 

upper :: (Maybe Limit) -> Integer
upper (Just l) = limit l
upper Nothing  = 0

lower :: (Maybe Limit) -> Integer
lower (Just l) = limit l
lower Nothing  = 0

limit :: Limit -> Integer
limit (LUnlimitedNatural v) = v  
limit (LInteger v)          = v 

lits [] = empty
lits (x:l) = lit x <> lits l

lit :: Literal -> Doc
lit (Literal n) = newLine <> doubleHorizontalSpace <> name n <> text ","

newLine = text "\n"
simpleHorizontalSpace = text "\t"
doubleHorizontalSpace = text "\t\t"
tripleHorizontalSpace = text "\t\t\t"

----------------------------
------------ C# ------------
----------------------------
usings = text "using Microsoft.EntityFrameworkCore;" <> newLine <>
  text "using System.Collections.Generic;"
    
builtInTypes = ["bool", "byte", "sbyte", "char", "decimal", "double", "float", "int", "uint", "long", "ulong", "object", "short", "ushort", "string"]
  
-- Check if the String (representing a type name) is within the built-in types of C#
isBuiltInTypes :: String -> Bool
isBuiltInTypes ty = (stringToLower ty) `elem` builtInTypes

-- Check if the (Maybe Type) is within the built-in types of C#
mIsBuiltInTypes :: (Maybe Type) -> Bool
mIsBuiltInTypes Nothing = True
mIsBuiltInTypes (Just (RefType idRef)) = isBuiltInTypes idRef 
mIsBuiltInTypes (Just (PrimitiveType hRef)) = isBuiltInTypes hRef

-- Check if the Type is within the built-in types of C#
fIsBuiltInTypes :: Type -> Bool
fIsBuiltInTypes (RefType idRef) = isBuiltInTypes idRef 
fIsBuiltInTypes (PrimitiveType hRef) = isBuiltInTypes hRef 

stringToLower :: String -> String
stringToLower s = (map toLower (trim s))

typeInCS :: String -> Doc
typeInCS []        = empty
typeInCS ty        = if (stringToLower ty) == "boolean" then text "bool"
  else if (stringToLower ty) == "integer" then text "int"
  else if isBuiltInTypes ty then text (stringToLower ty) else text ty

contextDeclaration = newLine <> simpleHorizontalSpace <> 
  text "public class DataBaseContext : DbContext {"

contextConfig n = newLine <> newLine <> doubleHorizontalSpace <> 
  text "protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder) {" <>
  newLine <> tripleHorizontalSpace <> 
  text "var connectionString = @" <> char '"' <> 
   text "Server=(localdb)\\mssqllocaldb; Database=" <> text n <> text "Data; Trusted_Connection=true" <> char '"' <> text ";" <>
  newLine <> tripleHorizontalSpace <> 
  text "optionsBuilder.UseSqlServer(connectionString);" <> 
  newLine <> doubleHorizontalSpace <> 
  text "}"

contextDeclarationEnd = newLine <> simpleHorizontalSpace <>
  text "}"

propertyId = newLine <> doubleHorizontalSpace <> 
  text "public int Id { get; set; }"

namespaceHeader name = newLine <> text "namespace" <+> text name <+> text "{" 
namespaceEnd = newLine <> text "}"

blockBegin = text "{"
blockEnd   = text "}"

operationBlock params = newLine <> doubleHorizontalSpace <> blockBegin <>
    (if thereIsReturnParam params then newLine <> tripleHorizontalSpace <> text "return null;"
    else empty) <>
  newLine <> doubleHorizontalSpace <> blockEnd

simpleType ty            = text ty
identifier id            = text id
capitalizedIdentifier id = text (capitalized id)
modifier mod             = text mod
name n                   = text n

abstract abs  = if abs then text "abstract "
  else empty

readOnly r    = if  r  then text "{ get; protected set; }"
  else text "{ get; set; }"

pp str = char '\'' <> text str <> char '\''

capitalized :: String -> String
capitalized (head:tail) = toUpper head : tail
capitalized [] = []

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
