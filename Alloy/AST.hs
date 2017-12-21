{-# LANGUAGE DeriveDataTypeable #-}

module Alloy.AST (Name, Module(..), Open(..),
                  SigQual(..), Paragraph(..), Scope(..), TypeScope(..),
                  Expr(..), Decl(..), LetDecl(..), Mult(..), Quant(..), Type(..), Super (..)) where

import Data.Generics

import Alloy.Relation
    
type Name = String -- not exactly the same...

data Module = Module (Maybe Name) [Open] [Paragraph]
                     deriving (Typeable, Data, Eq, Show)                   

data Open = Open Bool Name (Maybe [Name]) (Maybe Name)
            deriving (Typeable, Data, Eq, Show)
                     
data SigQual = Abstract | Private | SLone | SOne | SSome
               deriving (Typeable, Data, Eq, Show)

data Super = TopLevel | Extends Name | Inside Name
             deriving (Typeable, Data, Eq, Show)

data Paragraph = FactDecl (Maybe Name) Expr
               | AssertDecl (Maybe Name) Expr
               | FunDecl Bool (Maybe Name) Name (Maybe [Decl]) Expr Expr
               | PredDecl Bool (Maybe Name) Name (Maybe [Decl]) Expr
               | SigDecl [SigQual] [Name] Super [Decl] (Maybe Expr)
               | EnumDecl Name [Name]
               | RunDecl (Maybe Name) Expr Scope
               | CheckDecl (Maybe Name) Expr Scope
                 deriving (Typeable, Data, Eq, Show)

data Scope = Scope (Maybe Integer) [TypeScope] (Maybe Integer)
             deriving (Typeable, Data, Eq, Show)

data TypeScope = TypeScope Bool Integer Name
                 deriving (Typeable, Data, Eq, Show)

data Decl = Decl Bool Bool [Name] Bool Expr
            deriving (Typeable, Data, Eq, Show)

data LetDecl = LetDecl Name Expr
             deriving (Typeable, Data, Eq, Show)

data Quant = QNo | QLone | QSome | QOne | QAll | QSum
             deriving (Typeable, Data, Eq, Show)

data Mult = MSome | MOne | MLone | MSet
           deriving (Typeable, Data, Eq, Show)

data Expr = Id Name (Maybe Type)
          | Num Integer (Maybe Type)
          | None (Maybe Type) 
          | Iden (Maybe Type)
          | Univ (Maybe Type)
          | Converse Expr (Maybe Type)
          | RTransClosure Expr (Maybe Type) 
          | TransClosure Expr (Maybe Type)
          | Cardinality Expr (Maybe Type)
          | Join Expr Expr (Maybe Type)
          | Intersection Expr Expr (Maybe Type)
          | Arrow Expr (Maybe Mult) (Maybe Mult) Expr (Maybe Type)
          | Plus Expr Expr (Maybe Type)
          | Minus Expr Expr (Maybe Type)
          | Override Expr Expr (Maybe Type)
          | DomRestriction Expr Expr (Maybe Type)
          | RanRestriction Expr Expr (Maybe Type)
          | BoxJoin Expr [Expr] (Maybe Type)
          | No Expr (Maybe Type)
          | Some Expr (Maybe Type)
          | Lone Expr (Maybe Type)
          | One Expr (Maybe Type)
          | Set Expr (Maybe Type)
          | Quant Quant [Decl] Expr (Maybe Type)
          | In Expr Expr (Maybe Type)
          | Equals Expr Expr (Maybe Type)
          | LT Expr Expr (Maybe Type)
          | LTE Expr Expr (Maybe Type)
          | GT Expr Expr (Maybe Type)
          | GTE Expr Expr (Maybe Type)
          | Not Expr (Maybe Type)
          | And Expr Expr (Maybe Type)
          | Implies Expr Expr (Maybe Type)
          | Iff Expr Expr (Maybe Type)
          | Or Expr Expr (Maybe Type)
          | Ifte Expr Expr Expr (Maybe Type)
          | Let [LetDecl] Expr (Maybe Type)
          | Comprehension [Decl] Expr (Maybe Type)
          | Block [Expr] (Maybe Type)
            deriving (Typeable, Data, Eq, Show)

data Type = Bool | Int | Relation (Relation Name)
            deriving (Typeable, Data, Eq, Show)
