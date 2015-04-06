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

typeTerm :: Parser Term
typeTerm = try $ do
  t  <- upper
  ts <- many alphaNum
  return (t:ts)

tyleType :: Parser Type
tyleType = try $ do
    char ':' >> spaces
    t <- typeTerm <|> unit
    ts <- arrowType
    return (makeType (t:ts))
  where
        makeType [t]    = Type t Unit
        makeType (t:ts) = Type t (makeType ts)
        arrowType = many $ (spaces >> string "->" >> spaces) *> typeTerm
        unit =  string "()" >> return []

function :: Parser Expr
function = try $ do
  lambda >> spaces
  t0 <- term
  t1 <- tyleType
  char '.' >> spaces
  exp <- expression
  return (Fun t0 t1 exp)

expression :: Parser Expr
expression =  choice [ function
                     , application
                     , variable
                     ]

program :: Parser [Expr]
program = many1 $ expression <* (spaces >> char '#')
