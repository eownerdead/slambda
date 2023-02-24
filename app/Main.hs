module Main where

import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String

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
  getLine >>= \input -> case parse (expr <* eof) "slambda" input of
    Left e -> print e
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

-- expr ::= abs | app
expr :: Parser Ast
expr = spaces *> (abs_ <|> app) <* spaces

-- abs ::= [\\^λ] ident "." expr
abs_ :: Parser Ast
abs_ =
  Abs
    <$> (oneOf "\\^λ" *> spaces *> ident)
    <*> (spaces *> char '.' *> expr)

-- app ::= factor+
app :: Parser Ast
app = foldl1 App <$> many1 factor

-- factor ::= var | "(" expr ")"
factor :: Parser Ast
factor =
  spaces *> var <* spaces
    <|> spaces *> char '(' *> expr <* char ')' <* spaces

-- var ::= ident
var :: Parser Ast
var = Var <$> ident

-- ident ::= [a-zA-Z] [0-9a-zA-Z]*
ident :: Parser String
ident = (:) <$> letter <*> many alphaNum

