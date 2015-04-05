module Tyle.Parsers where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Control.Monad (liftM)

import Tyle.Grammar

lambda :: Parser String
lambda = choice $ map string [ "lambda"
                             , "\\"
                            , "\955"
                            ]

term :: Parser Term
term = do
  x  <- lower
  xs <- many (alphaNum <|> oneOf "_-><")
  return (x:xs)

variable :: Parser Expr
variable = liftM Var term

application :: Parser Expr
application = try $ do
  t0 <- term
  spaces
  t1 <- term
  return (App t0 t1)

function :: Parser Expr
function = do
  lambda
  spaces
  t <- term
  char '.'
  spaces
  exp <- expression
  return (Fun t exp)

assignment :: Parser Expr
assignment = try $ do
  t <- term
  spaces >> char '=' >> spaces
  e <- expression
  return (Asn t e)

expression :: Parser Expr
expression =  choice [ function
                     , assignment
                     , application
                     , variable
                     ]

program :: Parser [Expr]
program = many1 $ expression <* (spaces >> char '#')
