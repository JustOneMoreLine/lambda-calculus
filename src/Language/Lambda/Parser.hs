module Language.Lambda.Parser (parseExpr) where

import Control.Monad
import Prelude hiding (abs, curry)

import Text.Parsec
import Text.Parsec.String

import Language.Lambda.Expression

parseExpr :: String -> Either ParseError (LambdaExpr String)
parseExpr = parse (whitespace *> expr <* eof) ""

expr :: Parser (LambdaExpr String)
expr = try app <|> term

term :: Parser (LambdaExpr String)
term = let' <|> abs <|> var <|> parens

-- a branch from parenthesis parsing (parens) it could be either a new term or
-- a cnumber or operations
parensTerm :: Parser (LambdaExpr String)
parensTerm = term <|> cnumber <|> operator

-- Adding extra parse for church numbers abstraction
cnumber :: Parser (LambdaExpr String)
cnumber = let num = (read (<$> numNopsIdentifier) :: Integer) in (numAbs num)

-- generate church numerals
numAbs 0 = Abs "s" (Abs "z" (Var "z"))
numAbs 1 = Abs "s" (Abs "z" (App (Var "s") (Var "z")))
numAbs x = Abs "s" (Abs "z" (applicationLoop x))
applicationLoop 1 = App (Var "s") (Var "z")
applicationLoop x = App (Var "s") (applicationLoop (x - 1))

-- Adding extra parse for operators addition and multiplication
operator :: Parser (LambdaExpr String)
operator = opAbs (<$> numNopsIdentifier)

-- generate operation abstraction
opAbs "+" = Abs "w" (Abs "y" (Abs "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
opAbs "*" = Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "z")))))

-- parsing numbers and operators
numNopsIdentifier :: Parser String
numNopsIdentifier = lexeme ((:) <$> first <*> many rest) where
	first = char '+' <|> char '*' <|> digit

var :: Parser (LambdaExpr String)
var = Var <$> identifier

abs :: Parser (LambdaExpr String)
abs = curry <$> idents <*> expr
  where idents = symbol '\\' *> many1 identifier <* symbol '.'
        curry = flip (foldr Abs)

app :: Parser (LambdaExpr String)
app = chainl1 term (return App)

let' :: Parser (LambdaExpr String)
let' = Let <$> ident <*> expr
  where ident = keyword "let" *> identifier <* symbol '='

parens :: Parser (LambdaExpr String)
parens = symbol '(' *> parensTerm <* symbol ')'

lexeme :: Parser a -> Parser a
lexeme p =  p <* whitespace

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many rest)
  where first = letter <|> char '_'
        rest  = first <|> digit

symbol :: Char -> Parser ()
symbol = void . lexeme . char

keyword :: String -> Parser ()
keyword = void . lexeme . string
