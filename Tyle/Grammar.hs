{-# Language DataKinds #-}
module Tyle.Grammar where

type Term = String

data Expr = Var Term
          | Fun Term Type Expr
          | App Term Term
          deriving (Show)

data Type = Type Term Type
          | Unit
          deriving (Show)

data Context = Empty
             | Context Expr Type
