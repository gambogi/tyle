module Tyle.Grammar where

type Term = String
data Expr = Var Term
          | Fun Term Expr
          | App Term Term
          | Asn Term Expr
    deriving (Show)

