{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Data.Syntax.DeepCalculator where

import Data.Parser.DeepNanoParsec
import Control.Applicative ((<|>))

data Expr where
  Add :: Expr -> Expr -> Expr
  Div :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Lit :: Int -> Expr

deriving instance Show Expr

int :: Parser Expr
int = do
  n <- number
  spaces
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
mulOp = (infixOp "*" Mul) <|> (infixOp "/" Div)
