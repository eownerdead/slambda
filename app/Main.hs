module Main where

import Control.Monad.Combinators
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Ast
  = Var String
  | Abs String Ast
  | App Ast Ast
  | Nat Int
  | LetIn String Ast Ast
  deriving (Eq)

instance Show Ast where
  show (Var x) = x
  show (Abs x m) = "\\" ++ x ++ ". " ++ show m
  show (App m n) = sm ++ " " ++ sn
    where
      sm = case m of
        (Abs _ _) -> "(" ++ show m ++ ")"
        _ -> show m
      sn = case n of
        (Var _) -> show n
        (Nat _) -> show n
        _ -> "(" ++ show n ++ ")"
  show (Nat n) = show n
  show (LetIn x m n) = "let " ++ x ++ " = " ++ show m ++ " in\n" ++ show n

main :: IO ()
main =
  getContents >>= \input -> case parse slambda "slambda" input of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> do
      let st = steps x
      mapM_ (\y -> putStrLn $ "= " ++ show y) st
      mapM_
        (\n -> putStrLn $ show n ++ " (nat)")
        (fromChurchNum $ last st)

steps :: Ast -> [Ast]
steps x = unfoldr steps' (Just x)
  where
    steps' (Just y)
      | y /= step y = Just (y, Just $ step y)
      | otherwise = Just (y, Nothing)
    steps' Nothing = Nothing

step :: Ast -> Ast
step = step' []
  where
    step' _ (Var x) = Var x
    step' vars (Abs x m) = Abs x (step' (x : vars) m)
    step' vars (App (Abs x m) n) = subst vars x n m
    step' vars (App (Nat n) m) = App (alphaConv vars $ toChurchNum n) m
    step' vars (App m n) = App (step' vars m) (step' vars n)
    step' _ (Nat n) = Nat n
    step' vars (LetIn x m n) = subst vars x m n

subst :: [String] -> String -> Ast -> Ast -> Ast
subst vars old new (Var x)
  | x == old = alphaConv vars new
  | otherwise = Var x
subst vars old new (Abs x m) =
  alphaConv vars $ Abs x (subst (x : vars) old new m)
subst vars old new (App m n) = App (subst vars old new m) (subst vars old new n)
subst _ _ _ (Nat n) = Nat n
subst vars old new (LetIn x m n) =
  LetIn x (subst vars old new m) (subst vars old new n)

alphaConv :: [String] -> Ast -> Ast
alphaConv _ (Var x) = Var x
alphaConv vars (Abs x m) = Abs x' (subst vars x (Var x') m)
  where
    x' = freshName vars x
alphaConv vars (App m n) = App (alphaConv vars m) (alphaConv vars n)
alphaConv _ (Nat n) = Nat n
alphaConv vars (LetIn x m n) = LetIn x (alphaConv vars m) (alphaConv vars n)

freshName :: [String] -> String -> String
freshName vars x
  | x `elem` vars = x ++ "'"
  | otherwise = x

fromChurchNum :: Ast -> Maybe Int
fromChurchNum (Abs f (Abs x n)) = fromChurchNum' n
  where
    fromChurchNum' (Var x')
      | x' == x = Just 0
    fromChurchNum' (App (Var m) n')
      | m == f = fmap (+ 1) (fromChurchNum' n')
    fromChurchNum' _ = Nothing
fromChurchNum _ = Nothing

toChurchNum :: Int -> Ast
toChurchNum n = Abs "s" (Abs "z" (toChurchNum' n))
  where
    toChurchNum' 0 = Var "z"
    toChurchNum' m = App (Var "s") (toChurchNum' (m - 1))

type Parser = Parsec Void String

kws :: [String]
kws = ["let", "in"]

-- slambda ::= letIn* expr
slambda :: Parser Ast
slambda = expr <* eof

-- expr ::= abs | app | letIn
expr :: Parser Ast
expr = abs_ <|> app <|> letIn

-- abs ::= [\\^λ] ident "." expr
abs_ :: Parser Ast
abs_ =
  Abs
    <$> (lexeme (oneOf "\\^λ") *> ident)
    <*> (symbol "." *> expr)

-- app ::= factor+
app :: Parser Ast
app = foldl1 App <$> some factor

-- letIn ::= "let" ident "=" expr "in" expr
letIn :: Parser Ast
letIn =
  LetIn
    <$> (symbol "let" *> ident)
    <*> (symbol "=" *> expr)
    <*> (symbol "in" *> expr)

-- factor ::= var | nat | "(" expr ")"
factor :: Parser Ast
factor =
  try var <|> nat <|> symbol "(" *> expr <* symbol ")"

-- var ::= ident
var :: Parser Ast
var = Var <$> ident

-- ident ::= [a-zA-Z] [0-9a-zA-Z]*
ident :: Parser String
ident = lexeme (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar <?> "identifier"
    check x = if x `elem` kws then fail "keyword" else return x

-- nat ::= [0-9]*
nat :: Parser Ast
nat = Nat <$> lexeme L.decimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")
