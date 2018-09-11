module Data.Syntax.Calculator where

import Data.Parsers.NanoParsec
import Control.Applicative ((<|>))

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul
