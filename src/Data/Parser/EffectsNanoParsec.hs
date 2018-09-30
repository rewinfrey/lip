{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, KindSignatures, ScopedTypeVariables #-}
module Data.Parser.EffectsNanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Internal

data Parser (m :: * -> *) a where
  Pure     :: a -> Parser m a
  Bind     :: Parser m a -> (a -> Parser m b) -> Parser m b
  Failure  :: Parser m a
  Combine  :: Parser m a -> Parser m a -> Parser m a
  Option   :: Parser m a -> Parser m a -> Parser m a
  Satisfy  :: (Char -> Bool) -> Parser m Char
  Spaces   :: Parser m String
  Reserved :: String -> Parser m String
  Parens   :: Parser m a -> Parser m a

instance Functor (Parser m) where fmap = liftA

instance Applicative (Parser m) where
  pure = Pure
  (<*>) = ap

instance Monad (Parser m) where
  (>>=) = Bind

instance MonadPlus (Parser m) where
  mzero = Failure
  mplus = Combine

instance Alternative (Parser m) where
  empty = mzero
  (<|>) = Option

-- Why can't I use pointfree here? When I try to write `send . Bind` in a pointfree
-- definition for `bind`, I end up with compile errors:
{- /Users/rewinfrey/Documents/projects/lip/src/Data/Parser/EffectsNanoParsec.hs:36:8: error:
    • Couldn't match type ‘Parser (Eff r) b0’ with ‘Eff r b’
      Expected type: Parser (Eff r) a
                     -> (a -> Parser (Eff r) b) -> Eff r b
        Actual type: Parser (Eff r) a -> m0 e0 (Parser (Eff r) b0)
    • In the expression: send . Bind
      In an equation for ‘bind’: bind = send . Bind
    • Relevant bindings include
        bind :: Parser (Eff r) a -> (a -> Parser (Eff r) b) -> Eff r b
          (bound at /Users/rewinfrey/Documents/projects/lip/src/Data/Parser/EffectsNanoParsec.hs:36:1)
-}
bind :: Member Parser r => Parser (Eff r) a -> (a -> Parser (Eff r) b) -> Eff r b
bind p f = send $ Bind p f

combine :: Member Parser r => Parser (Eff r) a -> Parser (Eff r) a -> Eff r a
combine p p' = send $ Combine p p'

satisfy :: Member Parser r => (Char -> Bool) -> Eff r Char
satisfy f = send $ Satisfy f

failure :: Member Parser r => Eff r a
failure = send Failure

char :: Member Parser r => Char -> Eff r Char
char = satisfy . (==)

string :: Member Parser r => String -> Eff r String
string [] = return mempty
string (c:cs) = char c >> string cs >> return (c:cs)

oneOf :: Member Parser r => String -> Eff r Char
oneOf = satisfy . flip elem

chainl :: (Member NonDet r, Member Parser r) => Parser (Eff r) a -> Parser (Eff r) (a -> a -> a) -> a -> Eff r a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Member Parser r => Parser (Eff r) a -> Parser (Eff r) (a -> a -> a) -> Eff r a
chainl1 p op = send $ do
  a <- p
  rest a
  where
    rest a  = recur a <|> return a
    recur a = do
      f <- op
      b <- p
      rest (f a b)

natural :: (Member NonDet r, Member Parser r) => Eff r Integer
natural = read <$> some (satisfy isDigit)

-- I think I have to make this whole operation one Eff, rather than try to decompose
-- the `p` effect within constructing the `token` Eff. This means adding a `Token`
-- constructor for `Parser m a`.
token :: (Member NonDet r, Member Parser r) => Parser (Eff r) a -> Eff r a
token p = send $ do
  a <- p
  return a

reserved :: Member Parser r => String -> Eff r String
reserved s = send $ Reserved s

-- spaces :: (Member NonDet r, Member Parser r) => Eff r String
-- spaces = many $ oneOf " \t\n\r"
-- I'm not sure how to make `spaces` an Eff because of how I use this combinator
-- within `token`. So I'm going to interpret it within the context of the `token` Eff.
spaces :: (Member NonDet r, Member Parser r) =>  Eff r String
spaces = many (oneOf " \t\n\r")

digit :: Member Parser r => Eff r Char
digit = satisfy isDigit

number :: (Member NonDet r, Member Parser r) => Eff r Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s <> cs)

-- Not sure what to do about this.
parens :: Parser m a -> Eff r a
parens p = send $ do
  reserved "("
  a <- return p
  reserved ")"
  return a

{-

I think using `Eff` to encode the base operations of NanoParsec is an interesting experiment,
but is fundamentally wrong.

Instead, I think maintaining the DeepEmbedding representation of `DeepNanoParsec` is the
preferable abstraction for defining a split between operations / combinators and
their evaluation semantics via interpreters.

But a DeepEmbedding and a system of operations defined as `Eff` appear to be similar
in that both define operations and encode them to be interpreted / evaluated.

I don't know what to do about `a <- p` in the `parens` combinator. That is wanting
to run the parser, but what does that mean when working in `Eff`?

-}

runParser :: Eff '[Parser] w -> IO w
runParser (Return x) = return x
runParser (E u q) = case decompose u of
              -- Right (Pure a) -> putStrLn msg  >> runTeletype (apply q ())
              -- Right GetLine        -> getLine      >>= \s -> runTeletype (apply q s)
              -- Right ExitSuccess    -> exitSuccess
              Left  _              -> error "This cannot happen"

--------------------------------------------------------------------------------
                          -- Effect Model --
--------------------------------------------------------------------------------
-- data Teletype s where
--   PutStrLn    :: String -> Teletype ()
--   GetLine     :: Teletype String
--   ExitSuccess :: Teletype ()
--
-- putStrLn' :: Member Teletype r => String -> Eff r ()
-- putStrLn' = send . PutStrLn
--
-- getLine'  :: Member Teletype r => Eff r String
-- getLine' = send GetLine
--
-- exitSuccess' :: Member Teletype r => Eff r ()
-- exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                     -- Effectful Interpreter --
--------------------------------------------------------------------------------
-- runTeletype :: Eff '[Teletype] w -> IO w
-- runTeletype (Val x) = return x
-- runTeletype (E u q) = case decompose u of
--               Right (PutStrLn msg) -> putStrLn msg  >> runTeletype (apply q ())
--               Right GetLine        -> getLine      >>= \s -> runTeletype (apply q s)
--               Right ExitSuccess    -> exitSuccess
--               Left  _              -> error "This cannot happen"

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
-- runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
-- runTeletypePure inputs req = reverse (go inputs req [])
--   where go :: [String] -> Eff '[Teletype] w -> [String] -> [String]
--         go _      (Val _) acc = acc
--         go []     _       acc = acc
--         go (x:xs) (E u q) acc = case decompose u of
--           Right (PutStrLn msg) -> go (x:xs) (apply q ()) (msg:acc)
--           Right GetLine        -> go xs     (apply q x) acc
--           Right ExitSuccess    -> go xs     (Val ())   acc
--           Left _               -> go xs     (Val ())   acc

-- Parsing an input stream will either yield a parsed result, or error by not
-- consuming the entire input stream, or error because of a parser error.
-- TODO: add more robust error information indicating where the error in the input
-- stream occurred. Either would let us track errors.
-- runListParser :: Eff '[Parser] -> String -> a
-- runListParser parser input =
--   case evalList parser input of
--     [(a,   [])]     -> a
--     [(_,   rs)]     -> error "Parser did not consume the entire stream."
--     _               -> error "Parser error."
--
-- TODO: Data Result a = Result { match :: a, rest :: String } deriving Functor
-- Then we can interpret the return type of `eval` to Result
-- Usually interpret your deeply embedded monad in terms of another monad.
-- evalList :: Eff '[Parser] -> String -> [(a, String)]
-- evalList p s = case p of
--   Pure c       -> pure (c, s)
--   Failure      -> mzero
--   Satisfy p    -> if null s then mzero else if p (head s) then pure (head s, tail s) else mzero
--   Bind p f     -> concatMap (\(a, s') -> evalList (f a) s') $ (evalList p s)
--   Combine p p' -> evalList p s <> evalList p' s
--   Option p p'  -> evalList p s <|> evalList p' s
--
-- sequenceExample :: Parser String
-- sequenceExample = do
--   result1 <- string "abc"
--   result2 <- string "def"
--   pure (result1 <> result2)
--
-- combineExample :: Parser String
-- combineExample = combine (string "abc") (string "def")
--
-- optionExample :: Parser String
-- optionExample = (string "abc") <|> (string "def")
--
-- mixExample :: Parser String
-- mixExample = do
--   result1 <- string "abc" <|> string "def"
--   result2 <- string "ghi"
--   result3 <- parens number
--   pure (result1 <|> result2 <|> (show result3))
