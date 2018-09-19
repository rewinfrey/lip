module Data.Parser.Parsec () where

-- import Text.Parsec

{-
After working through a shallow and deep embedding for NanoParsec,
let's use the Haskell library Parsec.

Parsec contains several combinators defined in NanoParsec, so the
interface and operations are familiar.

The biggest difference is Parsec's ability to define a custom lexer
to identify tokens and keywords.
-}

import Text.Parsec ((<|>))
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr  as Ex
import qualified Text.Parsec.Char  as Char
import Text.Parsec.String
import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{"
  , Tok.commentEnd      = "}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = Char.letter
  , Tok.identLetter     = Char.alphaNum <|> Char.oneOf "_'"
  , Tok.opStart         = Char.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = Char.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reservedNames :: [String]
reservedNames = []

reservedOps :: [String]
reservedOps = []

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)
