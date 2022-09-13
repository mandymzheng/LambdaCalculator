module Parse where

import Text.Parsec ( many1, (<|>), parse, ParseError )
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef) -- language definition choosen for makeTokenParser
import Text.Parsec.Token (makeTokenParser)
import Control.Monad.Combinators.Expr ( Operator(InfixL), makeExprParser )
import qualified Text.Parsec.Token as T
import GHC.Base (Symbol)

-- define new data types for lambda calculus 
data Expr 
    = Var String                    -- Variable
    | Lambda String Expr            -- Lambda Abstraction
    | App Expr Expr                 -- Application
    | Num Integer                   -- Integer variables
    | Add Expr Expr                 -- Adding expressions
    | Multiply Expr Expr            -- Multiplying expressions
    | Divide Expr Expr              -- Divding expressions
    | Subtract Expr Expr            -- Subtracting expressions
    deriving (Eq, Ord, Show)

-- parens lexeme parser passes the body of text enclosed in parenthesis
parens :: Parser a -> Parser a 

-- identifer lexeme parser parses identifier strings which are not reserved words 
identifier :: Parser String

-- reservedOp is a lexeme parser which returns symbols and check it is not a prefix of a valid operator 
reservedOp :: String -> Parser ()

-- natural is a lexeme parser which returns natural numbers
natural :: Parser Integer 

-- symbol lexeme parser parses strings and skips trailing white space
symbol :: String -> Parser String

-- makeTokenParser creates a GenTokenParsers record that define lexical parsers (lexers)
-- used record pattern matching to reduce repeated code
T.TokenParser { T.parens = parens,
                T.identifier = identifier,
                T.reservedOp = reservedOp,
                T.natural = natural,
                T.symbol = symbol
              } = makeTokenParser haskellDef

-- Table of list of operators ordered in descending precedence
table :: [[Operator Parser Expr]]
table = [[binary "*" Multiply, binary "/" Divide],
         [binary "+" Add, binary "-" Subtract]]

-- specifies which operators work on expr type. 
-- All operators are binary, left-associative infix, operators
binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- a variable is a identifer 
variable :: Parser Expr
variable = do 
    Var <$> identifier 

-- define natural numbers as type Num
number :: Parser Expr
number = do
    Num <$> natural

-- return data type Lamda String Expr, using pattern matching of the expression
abstr :: Parser Expr -> Parser Expr
abstr expr = do
    reservedOp "\\"
    name <- identifier
    reservedOp "."
    Lambda name <$> expr

-- All non-application data types
nonApp :: Parser Expr
nonApp = parens expr <|> abstr expr <|> variable <|> number

-- return data type App Expr Expr, by using foldl1 on the nonApp and table terms
expr :: Parser Expr
expr = do 
    y <- many1 (makeExprParser nonApp table)
    return (foldl1 App y)

-- evaluate the input and return expression if valid or error message
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "<stdin>"