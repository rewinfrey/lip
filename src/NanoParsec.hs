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
