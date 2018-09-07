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

-- The MonadPlus type class is for Monads that also support choice and failure.
instance MonadPlus Parser where
  mzero = failure
  mplus = combine
-- Question: http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad.html#t:MonadPlus
-- indicates that the default implementation for `mzero` is `empty`. The Hackage
-- docs indicate that `empty` is a method belonging to the Alternative type class.
-- Is there a significant reason why this MonadPlus instance overrides the default
-- definition of `mzero` for `failure`, yet the Alternative instance defines empty in terms
-- of mzero?
-- Why not instead use the default definition of `mzero` (i.e. `empty`) and define
-- `empty` as `failure`?
-- My best guess is Alternative is a lower level abstraction because its super class
-- is Applicative, whereas MonadPlus is a higher level abstraction because its super
-- class is Monad. I think it's better to define methods of lower level abstractions
-- (i.e. Alternative's `empty` in this context) in terms of its equivalent higher
-- level abstraction (i.e. MonadPlus' `mzero`).
-- But that doesn't seem to jive with how `MonadPlus` defines `mzero` by default in terms
-- of `Alternative`'s `empty`.
-- My next best guess is that because we're wanting to define Monoidal operations
-- for the `Monad Parser` via the `MonadPlus Parser` type class instance, then it's
-- preferable to keep `mzero` and `mplus` at the same level of abstraction
-- (i.e. in terms of the user defined `failure` and `combine` functions, respectively).
-- Maintaining the default definition of `MonadPlus`'s `mzero` method for this instance
-- means we have `mplus` referring to a user defined function, but that we have to follow
-- `Alternative Parser`'s definition of `empty` to find `failure`.
-- It almost seems like we could use an `ApplicativePlus` type class that lets us keep
-- these Monoidal operations at the same level of abstraction (i.e. super class for both
-- `Alternative` and `ApplicativePlus` is `Applicative`).
-- TODO: Express this without `MonadPlus` by using only `Alternative`, `Semigroup`, and `Monoid`.
-- TODO: Explore defining the ideas of mzero and mplus in the semilattices package (thinking `Join` and `Lower`).

-- Alternative is a Monoid on Applicative Functors (Parser). Minimal complete
-- definition includes `empty` and `(<|>)`, although `some` and `many` can be defined.
instance Alternative Parser where
  empty = mzero
  (<|>) = option
  -- `some` and `many` are both derived automatically. These would be their definitions:
  --
  -- One or more.
  -- some :: f a -> f [a]
  -- some v = some_v
  --  where some_v = (:) <$> v <*> many_v
  --        many_v = some_v <|> pure []
  --
  -- Zero or more.
  -- many :: f a -> f [a]
  -- many v = many_v
  --  where many_v = some_v <|> pure []
  --        some_v = (:) <$> v <*> many_v

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s <> parse q s)
-- Nit: The tutorial defines this as: combine p q = Parser (\s -> parse p s ++ parse q s),
-- but it's safer and arguably more morally correct to prefer `(<>)` over `(++)`.

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

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

-- Tests a predicate function on the head of the input String, returning a parsed
-- result or the failure Parser (if the predicate failed). The failure Parser
-- accepts all input but always returns the empty list.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure

{-
With do syntactic sugar:
satisfy p = do
  c <- item
  if p c
  then unit c
  else failure
-}

{-
With bind operator:
satisfy p = item >>= \c -> if p c then unit c else failure
-}

-- Returns a parser that succeeds when given a Char that's an element of a provided list of Chars.
-- This is most helpful for sets of Chars (e.g. a list of whitespace Chars).
oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

-- chainl allows parsing expressions like `1 + 2 + 3`. The first Parser is used to
-- parse an operand and the second Parser is used to parse the operator.
-- chainl relies on recursion via the chainl1 function, which flattens out left
-- associated recursive productions, and stops after parsing the final operand
-- in a operator expression.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a = recur a <|> return a
    recur a = do
      f <- op
      b <- p
      rest (f a b)
