{-# LANGUAGE DeriveDataTypeable #-}

module OCL.AST where

import Data.Generics

type Name = String

data Package = Package Name [Constraint]
               deriving (Typeable, Data, Eq, Show)

data Constraint = InvDecl Name Expr
                | QueryDecl Name Name [Decl] Name Expr
                | OpDecl Name Name [Decl] [Expr] [Expr]
                  deriving (Typeable, Data, Eq, Show)

data Decl = Decl Name Name
            deriving (Typeable, Data, Eq, Show)

data Expr = Includes Expr Expr
          | ForAll Expr Name Expr
          | Exists Expr Name Expr
          | IsEmpty Expr
          | Bot
          | Top
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Implies Expr Expr
          | Ifte Expr Expr Expr
          | Comp Op Expr Expr
          | Size Expr
          | Sum Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Id Name
          | Num Integer
          | Const Name Name
          | Navigate Bool Expr Name [Expr]
          | Call Bool Expr Name [Expr]
          | AsSet Expr
          | Union Expr Expr
          | Intersection Expr Expr
          | AllInstances Name
          | Select Expr Name Expr
          | Reject Expr Name Expr
          | Collect Expr Name Expr
          | Closure Expr Name Expr
          | OclAsType Expr Name
          | OclIsKindOf Expr Name
            deriving (Typeable, Data, Eq, Show)

data Op = EQ | GT | LT | GTE | LTE
          deriving (Typeable, Data, Eq, Show)

