module Data.Parser.SimpleLanguage (parseExpr) where

{-
After working through a shallow and deep embedding for NanoParsec,
let's use the Haskell library Parsec.

Parsec contains several combinators defined in NanoParsec, so the
interface and operations are familiar.

The biggest difference is Parsec's ability to define a custom lexer
to identify tokens and keywords.
-}

import Text.Parsec ((<|>), eof, parse)
import qualified Text.Parsec.Error as Err
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr  as Ex
import qualified Text.Parsec.Char  as Char
import Text.Parsec.String
import Data.Functor.Identity
import qualified Data.Syntax.SimpleLanguage as SL

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

-- Parsers
prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

table :: Ex.OperatorTable String () Identity SL.Expr
table =
  [
    [
      prefixOp "succ" SL.Succ
    , prefixOp "pred" SL.Pred
    , prefixOp "iszero" SL.IsZero
    ]
  ]

ifthen :: Parser SL.Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (SL.If cond tr fl)

-- Constants
true, false, zero :: Parser SL.Expr
true  = reserved "true"  >> return SL.Tr
false = reserved "false" >> return SL.Fl
zero  = reserved "0"     >> return SL.Zero

expr :: Parser SL.Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser SL.Expr
factor =  true
      <|> false
      <|> zero
      <|> ifthen
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either Err.ParseError SL.Expr
parseExpr s = parse (contents expr) "<stdin>" s
