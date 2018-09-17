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

