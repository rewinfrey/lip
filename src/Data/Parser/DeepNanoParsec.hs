{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Data.Parser.DeepNanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

data Parser a where
  Item    :: Parser Char
  FMap    :: (a -> b) -> Parser a -> Parser b
  Pure    :: a -> Parser a
  Ap      :: Parser (a -> b) -> Parser a -> Parser b
  Bind    :: Parser a -> (a -> Parser b) -> Parser b
  Failure :: Parser a
  Combine :: Parser a -> Parser a -> Parser a
  Option  :: Parser a -> Parser a -> Parser a
  Satisfy :: (Char -> Bool) -> Parser Char

instance Functor Parser where fmap = FMap

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

-- TODO: Data Result a = Result { match :: a, rest :: String } deriving Functor
-- Then we can interpret the return type of `eval` to Result
eval :: Parser a -> String -> [(a, String)]
eval p s = case p of
  Pure c       -> pure (c, s)
  Item         -> if null s then mzero else pure (head s, tail s)
  Failure      -> mzero
  FMap f p'    -> fmap (\(a, s') -> (f a, s')) (eval p' s)
  Ap pf p      -> [(f a, s'') | (f, s') <- eval pf s, (a, s'') <- eval p s']
  Satisfy p    -> if null s then mzero else if p (head s) then pure (head s, tail s) else mzero
  Bind p f     -> concatMap (\(a, s') -> eval (f a) s') (eval p s)
  Combine p p' -> eval p s <> eval p' s
  Option p p'  -> eval p s <|> eval p' s

