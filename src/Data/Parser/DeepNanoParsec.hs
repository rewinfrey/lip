{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Data.Parser.DeepNanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

data Parser a where
  Pure    :: a -> Parser a
  Ap      :: Parser (a -> b) -> Parser a -> Parser b
  Bind    :: Parser a -> (a -> Parser b) -> Parser b
  Failure :: Parser a
  Combine :: Parser a -> Parser a -> Parser a
  Option  :: Parser a -> Parser a -> Parser a
  Satisfy :: (Char -> Bool) -> Parser Char

instance Functor Parser where fmap = liftA

instance Applicative Parser where
  pure = Pure
  (<*>) = Ap

instance Monad Parser where
  (>>=) = Bind

instance MonadPlus Parser where
  mzero = Failure
  mplus = Combine

instance Alternative Parser where
  empty = mzero
  (<|>) = Option

bind :: Parser a -> (a -> Parser b) -> Parser b
bind = Bind

combine :: Parser a -> Parser a -> Parser a
combine = Combine

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Satisfy

failure :: Parser a
failure = Failure

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string [] = return []
string (c:cs) = char c >> string cs >> return (c:cs)

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a  = recur a <|> return a
    recur a = do
      f <- op
      b <- p
      rest (f a b)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

token :: Parser a -> Parser a
token p = do
  spaces
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved = token . string

spaces :: Parser String
spaces = many $ oneOf " \t\n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s <> cs)

parens :: Parser a -> Parser a
parens p = do
  reserved "("
  a <- p
  reserved ")"
  return a

-- TODO: Data Result a = Result { match :: a, rest :: String } deriving Functor
-- Then we can interpret the return type of `eval` to Result
-- Usually interpret your deeply embedded monad in terms of another monad.
eval :: Parser a -> String -> [(a, String)]
eval p s = case p of
  Pure c       -> pure (c, s)
  Failure      -> mzero
  Ap pf p      -> [(f a, s'') | (f, s') <- eval pf s, (a, s'') <- eval p s']
  Satisfy p    -> if null s then mzero else if p (head s) then pure (head s, tail s) else mzero
  Bind p f     -> concatMap (\(a, s') -> eval (f a) s') (eval p s)
  Combine p p' -> eval p s <> eval p' s
  Option p p'  -> eval p s <|> eval p' s

sequenceExample :: Parser String
sequenceExample = do
  result1 <- string "abc"
  result2 <- string "def"
  pure (result1 <> result2)

combineExample :: Parser String
combineExample = combine (string "abc") (string "def")

optionExample :: Parser String
optionExample = (string "abc") <|> (string "def")

mixExample :: Parser String
mixExample = do
  result1 <- string "abc" <|> string "def"
  result2 <- string "ghi"
  result3 <- parens number
  pure (result1 <|> result2 <|> (show result3))
