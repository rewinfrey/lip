module Data.Interpreter.SimpleLanguage where

import Data.Syntax.SimpleLanguage
import Data.Parser.SimpleLanguage
import Data.Maybe
import System.Console.Haskeline
import Control.Monad.IO.Class

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr          = True
isVal Fl          = True
isVal t | isNum t = True
isVal _           = False

eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval' t)
  If Tr c _                 -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing

-- Evaluates to the "normal form" of a given Expr.
nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"

-- runInputT :: Settings IO -> InputT IO a -> IO a
-- getInputLine :: String -> InputT IO (Maybe String)

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> print $ eval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Repl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
