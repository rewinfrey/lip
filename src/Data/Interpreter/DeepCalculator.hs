module Data.Interpreter.DeepCalculator where

import Control.Monad (forever)
import Data.Parser.DeepNanoParsec
import Data.Syntax.DeepCalculator

evalInt :: Expr -> Int
evalInt ex = case ex of
  Add a b -> evalInt a + evalInt b
  Div a b -> (evalInt a) `div` (evalInt b)
  Mul a b -> evalInt a * evalInt b
  Sub a b -> evalInt a - evalInt b
  Lit n   -> n

run :: String -> Expr
run = runListParser expr

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ evalInt $ run a
