module Data.Interpreter.Calculator where

import Control.Monad (forever)
import Data.Parser.ShallowNanoParsec
import Data.Syntax.Calculator

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Div a b -> (eval a) `div` (eval b)
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
