{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

{-
  NanoParsec is based entirely on http://dev.stephendiehl.com/fun/002_parsers.html.
  The approach outlined here is similar to that of other parser combinator libraries.
  Parser combinators in general are a collection of functions that accept as input
  a parser and whose return value is a new parser.

  A parser is a function that accepts String input and whose result value is structured
  to indicate where parsing succeeded, stopped, and what if any of the input String is remaining for a subsequent parser.

  Parse combinators then are a collection of higher-order functions whose input
  are parsers and whose return values are also parsers. This allows for the composition
  of multiple parsers (higher-order functions) when consuming an input String.
  This approach is convenient for many reasons, but chief among them is the ability
  to embed logic within individual parsers for handling input and allowing for composition
  within an individual parser.
-}

import Data.Char
import Control.Monad
import Control.Applicative

-- Parsing an input stream will either yield a parsed result, or error by not
-- consuming the entire input stream, or error because of a parser error.
-- TODO: add more robust error information indicating where the error in the input
-- stream occurred. Either would let us track errors.
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_,   rs)] -> error "Parser did not consume the entire stream."
    _           -> error "Parser error."

-- A Parser is a function that takes an input String, and returns a list of pairs
-- where `a` is the result, and String is the remaining string to parse.
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- fmap over a Parser yields a new Parser that applies the supplied function to
-- the result of the initial Parser.
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- <*> between two Parsers yields a new Parser whose result is the application of the
-- function returned from evaluating the first Parser to the result of the second Parser
-- whose input String is the returned String from the first Parser. Sequencing here
-- ensures that the function applied to the second Parser's result is the function
-- returned from the first Parser, and for consistency, the input String given to the
-- second Parser is the remainder stream from the first Parser's return value.
instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- We express the Monad interface for Parser in terms of `bind` and `unit`.
instance Monad Parser where
  return = unit
  (>>=)  = bind

-- Injects a single pure value as the result without reading from the input stream.
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- Composes one parse operation with a second parse operation. Because the result
-- of a parser is a list of pairs, `concatMap` is used to map over the pairs producing
-- a new flat list of pairs.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- Retrieves the next Char from the input String.
item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]
