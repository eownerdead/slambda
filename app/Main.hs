module Main where

import Control.Monad
import Control.Monad.Combinators
import Data.List
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Ast
  = Var String
  | Abs String Ast
  | App Ast Ast
  deriving (Eq)

instance Show Ast where
  show (Var x) = x
  show (Abs x m) = "\\" ++ x ++ ". " ++ show m
  show (App m@(Var _) n) = show m ++ " " ++ show n
  show (App m n) = "(" ++ show m ++ ") " ++ show n

main :: IO ()
main =
  T.getContents >>= \input -> case parse (expr <* eof) "slambda" input of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> forM_ (steps x) print

steps :: Ast -> [Ast]
steps x = unfoldr steps' (Just x)
  where
    steps' (Just y)
      | y /= step y = Just (y, Just $ step y)
      | otherwise = Just (y, Nothing)
    steps' Nothing = Nothing

step :: Ast -> Ast
step (Var x) = Var x
step (Abs x m) = Abs x (step m)
step (App (Abs x m) n) = subst x n m
step (App m n) = App (step m) (step n)

subst :: String -> Ast -> Ast -> Ast
subst old new (Var x)
  | x == old = new
  | otherwise = Var x
subst old new (Abs x m) = Abs x' (subst old new m')
  where
    x' = x ++ "'"
    m' = subst x (Var x') m
subst old new (App m n) = App (subst old new m) (subst old new n)

type Parser = Parsec Void Text

-- expr ::= abs | app
expr :: Parser Ast
expr = ws *> (abs_ <|> app) <* ws

-- abs ::= [\\^λ] ident "." expr
abs_ :: Parser Ast
abs_ =
  Abs
    <$> (oneOf "\\^λ" *> ws *> ident)
    <*> (ws *> char '.' *> expr)

-- app ::= factor+
app :: Parser Ast
app = foldl1 App <$> some factor

-- factor ::= var | "(" expr ")"
factor :: Parser Ast
factor =
  ws *> var <* ws
    <|> ws *> char '(' *> expr <* char ')' <* ws

-- var ::= ident
var :: Parser Ast
var = Var <$> ident

-- ident ::= [a-zA-Z] [0-9a-zA-Z]*
ident :: Parser String
ident = (:) <$> letterChar <*> many alphaNumChar <?> "identifier"

ws :: Parser ()
ws = hidden space

