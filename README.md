# Modified Lambda Calculus

This project is a modified application that base from lambda calculus originally made by [@sgillespie](https://github.com/sgillespie).
This modified lambda calculus is made for in hopes to fulfill a required task in my Functional Programming course.

# Introduction to Lambda Calculus

A full introduction for lambda calculus can be found on the original project [here](https://github.com/sgillespie/lambda-calculus).
For this introduction I will focus on how to install and use the modified lambda calculus, and explain how the modified lambda calculus works.

# Introduction to Modified Lambda Calculus

This modified lambda calculus supports integer and arithmetic operation, addition and multiplication, as inputs.
It will use Church's Numerals to represent integers.

## How to Install

1. Have GHC >= 8 and stack installed
2. run

    stack build
    
3. run 

    stack install


## How to use

After you have successfully install the app, you can start the app by running

    lambda-calculator

A prompt would show up and you can start typing. Some examples:

    λ > \x. x
    λx. x
    λ > \x y. x y
    λx. xy

To use integers and arithmetic operation as inputs, you need to use them between parenthesis.
This feature supports positive integers only.

    λ > (2)
    λs z. s (s z)
    λ > (18)
    λs z. s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))
    λ > (+)
    λw y x. y (w y x)
    λ > (*)
    λx y z. x (y z)
    λ > (2)(+)(7)
    λy x. y (y (y (y (y (y (y (y (y x))))))))
    λ > (*)(2)(7)
    λz x. z (z (z (z (z (z (z (z (z (z (z (z (z (z x)))))))))))))

# How does this mod works

In the original lambda calculus, it uses 4 data type to represent lambda expressions, which are:
 * App (Application)
 * Abs (Abstraction)
 * Var (dependent variable)
 * Let (independent variable)
 
### [Expression.hs](src/Language/Lambda/Expression.hs)

    data LambdaExpr name
    = Var name
    | App (LambdaExpr name) (LambdaExpr name)
    | Abs name (LambdaExpr name)
    | Let name (LambdaExpr name)
    deriving (Eq, Show)

So to support integers and arithmetic operation as inputs, I add some modification in the parsing logic
to accept those inputs and interpreted them into lambda expression.

Since I planned to use parenthesis to mark integers or operations inputs and this parses uses Parsec, a monadic
haskell parser, I add extra parsing options in the parenthesis parser to accept numbers (cnumber) or arithemetic
operations (operator)

### [Parser.hs](src/Language/Lambda/Parser.hs)

    parens :: Parser (LambdaExpr String)
    parens = symbol '(' *> expr <* symbol ')'
    
    expr :: Parser (LambdaExpr String)
    expr = try app <|> term <|> cnumber <|> operator

cnumber would parse in digits and return a LambdaExpr in Church Numerals' form
from the function numAbs.

### [Parser.hs](src/Language/Lambda/Parser.hs)
    
    -- Adding extra parse for church numbers abstraction
    cnumber :: Parser (LambdaExpr String)
    cnumber = do
	    num <- many1 digit
	    return(numAbs (read num :: Integer))
    
    -- generate church numerals
    numAbs 0 = Abs "s" (Abs "z" (Var "z"))
    numAbs 1 = Abs "s" (Abs "z" (App (Var "s") (Var "z")))
    numAbs x = Abs "s" (Abs "z" (applicationLoop x))
    applicationLoop 1 = App (Var "s") (Var "z")
    applicationLoop x = App (Var "s") (applicationLoop (x - 1))
    
 Simillarly, operations also parse in an arithmetic operation symbol and
 return a LambdaExpr of those operations
 
 ### [Parser.hs](src/Language/Lambda/Parser.hs)
 
    -- Adding extra parse for operators addition and multiplication
    operator :: Parser (LambdaExpr String)
    operator = do
	    op <- anyChar
	    return (opAbs op)

    -- generate operation abstraction
    opAbs '+' = Abs "w" (Abs "y" (Abs "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
    opAbs '*' = Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y") (Var "z")))))
