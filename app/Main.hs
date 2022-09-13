module Main where

import Parse ( parseExpr, Expr(..), variable )
import Calculus ( rep )

main :: IO b
main = do
    putStrLn "Enter an untyped λ expression:"
    putStrLn "Please separate lambda variables with () e.g. (\\x.(\\y.x)) x instead of (\\x.\\y.x) x"
    string <- getLine -- input from terminal
    putStrLn "Result:" 
    case parseExpr string of
      Left err -> error $ show err -- output error message
      Right x -> rep x -- call rep from calculus to begin lambda calculus
    main