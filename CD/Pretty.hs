module CD.Pretty (pretty) where

import Text.PrettyPrint
import Prelude hiding (EQ,GT,LT)
import CD.AST


pretty :: Package -> String
pretty = render . spec

spec :: Package -> Doc
spec (Package n elems) = xmiHd <> (modelHd n) <+> (text "") 
			 $$ vcat (punctuate (text "") (map (\x -> pack x) elems) ) <+> modelEnd 

pack :: Packaged  -> Doc
pack (Class n abs ows)  = packBegin <+> xmiTy "Class" <+> 
                          xmiId n <+> xmiName n <+> xmiAbs abs <+> text ">"  <> text "" 
			  $$ vcat (punctuate (text "") (map (\x -> owCl x) ows) ) <> packEnd
pack (Enumeration n ls) = packBegin <+> xmiTy "Enumeration" <+> xmiId n <+> xmiName n <> text ">" <>
			  lits ls <> packEnd


owCl :: Owned -> Doc
owCl (Property n r ty upp low qs) = 
                          propBegin <+> xmiTy "Property" <+> xmiId n <+> xmiName n <+> xmiRead r <> 
			  text ">" <> tagTyp ty <>
			  upper upp <> lower low <> quals qs <> propEnd

owCl (Generalization gl) = 
			  text "\n\t<generalization" <+> xmiTy "Generalization" <+>
			  xmiGrl gl <+> text "/>"
owCl (Operation n ps   ) =  
			  opBegin <+> xmiTy "Operation" <+> xmiName n <> text ">" <> params ps <> opEnd


params :: [Parameter] -> Doc
params []    = text ""
params (x:l) = param x  <> params l 

param :: Parameter -> Doc
param (InOut n t)      = paramBegin <+> xmiTy "Parameter" <+> xmiName n <+> fTyp t <> text "/>"
param (Return t up lw) = paramBegin <+> xmiTy "Parameter" <+> fTyp t <+> xmiDir "return" <> text ">" <>
			 upper up <> lower lw <> paramEnd


tagTyp :: (Maybe Type) -> Doc
tagTyp Nothing = text ""
tagTyp (Just (RefType idRef)) =
			 text "\n\t\t<type xmi:idref=" <> char '"' <> text idRef  <> char '"' <> text "/>" 
tagTyp (Just (PrimitiveType hRef)) = 
			 text "\n\t\t<type xmi:type=" <> char '"' <> text "uml:PrimitiveType"  <> 
			 char '"' <+> text "href=" <> char '"' <> text hRef <> char '"' <> text "/>" 

fTyp :: Type -> Doc
fTyp (RefType idRef) = text "type=" <> pp idRef
fTyp (PrimitiveType hRef) = text "" 


quals :: [Qualified] -> Doc
quals [] = text ""
quals (x:l) = qual x <> quals l

qual :: Qualified -> Doc
qual (Qualified n t) = qualBegin <+> xmiName n <+> fTyp t <+> text "/>"


upper :: (Maybe Limit) -> Doc
upper (Just l) = text "\n\t\t\t\t<upperValue" <+> limit l <+> text "/>"
upper Nothing  = text ""

lower :: (Maybe Limit) -> Doc
lower (Just l) = text "\n\t\t\t\t<lowerValue" <+> limit l <+> text "/>"
lower Nothing  = text ""

limit :: Limit -> Doc
limit (LUnlimitedNatural v) = xmiTy "LiteralUnlimitedNatural" <+> xmiMul v  
limit (LInteger v)          = xmiTy "LiteralInteger" <+> xmiMul v 



membs [] = text ""
membs (x:l) = memb x <> membs l

memb :: MemberEnd -> Doc
memb x = text "\n <memberEnd" <+> xmiIdRef x <> text "/>"

lits [] = text ""
lits (x:l) = lit x <> lits l

lit :: Literal -> Doc
lit (Literal n) = text "\n\t\t\t <ownedLiteral" <+> xmiTy "EnumerationLiteral" <+> xmiName n <> text "/>"


mTyp Nothing  = text ""
mTyp (Just n) = text "type=" <> pp n


-----------------------------
--------- XMI ---------------
--------------------------


xmiHd = text "<?xml version=" <> char '\'' <> text "1.0" <> char '\'' <+> 
	text "encoding=" <> char '\'' <> text "UTF-8" <> char '\'' <> text "?>"

xmlns = text "xmi:version=" <> char '"' <> text "2.1" <> char '"' <+> 
	text "xmlns:xmi=" <> char '"' <> text "http://schema.omg.org/spec/XMI/2.1" <> char '"' <+>
	text "xmlns:uml=" <> char '"' <> text "http://www.eclipse.org/uml2/3.0.0/UML" <> char '"'

xmiEnd = text "\n </xmi:XMI>"

xmiBool = char '"' <> text "pathmap://UML_METAMODELS/UML.metamodel.uml#Boolean" <> char '"'


---------  Model --------------

modelHd name = 
  text "\n <uml:Package" <+> xmlns <+> text "name=" <> pp name <> text ">" <+> text "\n\t\t"
 

modelEnd = text "\n" <+> text "</uml:Package>"


packBegin  = text "\n <packagedElement"
packEnd    = text "\n </packagedElement>"

propBegin  = text "\n\t <ownedAttribute" 
propEnd    = text "\n\t </ownedAttribute>"

opBegin    = text "\n\t <ownedOperation" 
opEnd      = text "\n\t </ownedOperation>"

paramBegin = text "\n\t\t <ownedParameter" 
paramEnd   = text "\n\t\t </ownedParameter>"

qualBegin  = text "\n\t\t <qualifier" 
qualEnd    = text "\n\t\t </qualifier>"



xmiTy ty = text "xmi:type=" <> pp ("uml:"++ty)
xmiId id = text "xmi:id="   <> pp id
xmiIdRef id = text "xmi:idref=" <> pp id

xmiName n = text "name=" <> pp n
xmiAss  a = text "association=" <> pp a
xmiAgg  a = text "aggregation=" <> pp a
xmiAssEnd a = text "associationEnd=" <> pp a
xmiGrl g = text "general=" <> pp g
xmiDir d = text "direction=" <> pp d


xmiAbs abs = if abs then text "isAbstract=" <> pp "true"
		    else text "isAbstract=" <> pp "false"
xmiRead r  = if  r  then text "isReadOnly=" <> pp "true"
		    else text "isReadOnly=" <> pp "false"

xmiMul v = text "value=" <> char '\'' <> integer v <> char '\''

pp str = char '\'' <> text str <> char '\''





