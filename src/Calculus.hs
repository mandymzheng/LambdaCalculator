module Calculus where
 
import Parse ( Expr(..) )
import Data.List ( (\\), nub, union )

-- Repeat this function until lambda expression is in the simplest form and print each step
rep :: Expr -> IO()
rep t | eval t == t                         = putStrLn (format t)                       -- Stop when we can step no further
      | otherwise                           = do putStrLn (format t); rep (eval t)      -- repeat as not complete but print out each line

-- Format parser output to lambda expressions
format :: Expr -> String
format (Lambda var expr)                    = "(\\" ++ var ++ "." ++ format expr ++ ")"
format (App expr1 expr2)                    = "(" ++ format expr1 ++ " " ++ format expr2 ++ ")"
format (Var a )                             = a
format (Num x)                              = show x
format (Add term1 term2)                    = "(" ++ format term1 ++ "+" ++ format term2 ++ ")"
format (Subtract term1 term2)               = "(" ++ format term1 ++ "-" ++ format term2 ++ ")"
format (Multiply term1 term2)               = "(" ++ format term1 ++ "*" ++ format term2 ++ ")"
format (Divide term1 term2)                 = "(" ++ format term1 ++ "/" ++ format term2 ++ ")"

-- Steps to simplify lambda calculus, the outermost expression will be evaluated first
eval :: Expr -> Expr  
eval (Lambda variable expr)                 = Lambda variable (eval expr) -- for lamda terms, evaluate the expression term
eval (App (Lambda variable expr) var2)      = betaSubst variable expr var2 -- sub second term into first where variable (x) is in expr
eval (App expr1 expr2)                      = App (eval expr1) expr2 -- start with evaluating the first expression
eval v@(Var x)                              = v -- pattermatching to pass in Var a
eval i@(Num x)                              = i -- pattermatching to pass in Num x
eval (Add x y)                              = Add (eval x) (eval y) -- for operators, both terms need to be evaluated during reduction
eval (Subtract x y)                         = Subtract (eval x) (eval y)
eval (Divide x y)                           = Divide (eval x) (eval y)
eval (Multiply x y)                         = Multiply (eval x) (eval y)


--Method which tries to replace a given variable with a new expression if the given variable appears in the existing expression
betaSubst :: String -> Expr -> Expr -> Expr
-- if the variable to change is equivelent to the expression variable, then subsitude with new expression
betaSubst x (Var v) newExpr 
    | x == v          = newExpr
    | otherwise       = Var v

-- if the variable to change (x) is equivealent to the variable in the lambda function, then remain the same
-- if the variable to change (x) is NOT equivalent to the variable in lambda and the variable is not a freeVariable, then subsitude the x variable in the expression with the new expression
-- if the variable to change (x) is a freeVariable, then perform alpha subsitution
betaSubst x l@(Lambda variable expr) newExpr
    | x == variable                                                        = l -- use @ to reduce retyping code
    | x /= variable && (variable `notElem` freeVariables newExpr)          = Lambda variable (betaSubst x expr newExpr)
    | variable `elem` freeVariables newExpr                                = betaSubst x (alphaSubst (rename variable) l) newExpr -- if the free variable is the same as the variable in Lambda expression then rename the lambda variable
    | otherwise                                                            = error "No futher substitutions possible"

-- betaSubst both terms in Application seperately
betaSubst x (App expr1 expr2) newExpr = App (betaSubst x expr1 newExpr) (betaSubst x expr2 newExpr)

-- An integer should never need to subsituded
betaSubst x (Num i) newExpr = Num i 

-- For Mathematical operators, beta-subsitude both terms seperately
betaSubst x (Add term1 term2) newExpr = Add (betaSubst x term1 newExpr) (betaSubst x term2 newExpr)
betaSubst x (Subtract term1 term2) newExpr = Subtract (betaSubst x term1 newExpr) (betaSubst x term2 newExpr)
betaSubst x (Multiply term1 term2) newExpr = Multiply (betaSubst x term1 newExpr) (betaSubst x term2 newExpr)
betaSubst x (Divide term1 term2) newExpr = Divide (betaSubst x term1 newExpr) (betaSubst x term2 newExpr)


-- Returns the list of all unbounded variables from a given expression
freeVariables :: Expr -> [String]
freeVariables (Num x) = ["x"] -- an integer and variable can be a free variable 
freeVariables (Var x) = [x] 
freeVariables (Lambda variable expr) = nub (freeVariables expr) \\ [variable] -- nub removed duplicates from a list, \\ is the set difference
freeVariables (App expr1 expr2) = freeVariables expr1 `union` freeVariables expr2 -- the conjunction of the free variables from expr1 and expr2
freeVariables _ = [] -- mathematical operators cannot be free variables

-- rename given variable by adding ! to the front
rename :: String -> String 
rename x = '!':x

-- if a free variable and lambda variable are equivalent, perform alpha subsitution with the lambda variable being the rename name
alphaSubst :: String -> Expr -> Expr
alphaSubst newVar (Lambda oldVar expr) = Lambda newVar (betaSubst oldVar expr (Var newVar)) 
-- No need for alpha subsitution on non-Lambda data types as no clash between lambda variable and free varaible
alphaSubst _ v@(Var _) = v
alphaSubst _ a@(App _ _ ) = a
alphaSubst _ i@(Num _) = i
alphaSubst _ o = o

--------------------------------------------------------------------------------------------------------------------------------------

---- UNIT TESTS
---- Most unit tests from: https://codegolf.stackexchange.com/questions/284/write-an-interpreter-for-the-untyped-lambda-calculus

--------------------------------------------------------------------------------------------------------------------------------------

-- (((\ x. (\ y. y)) (\ a. a)) (\ b. b))
-- (λ b. b)

-- (((\ x. (\ y. x)) (\ a. a)) (\ b. b))
-- (λ a. a)

-- ((\ x. x) (\ y. (\ z. z)))
-- (λ y. (λ z. z))

-- (\ x. ((\ y. y) x))
-- (λ x. x)

-- ((\ x. (\ y. x)) (\ a. a))
-- (λ y. (λ a. a))

-- ((\ x. (\ y. y)) (\ a. a))
-- (λ y. y)

-- (\y.(\a.a))(\b.b)
-- (\a.a)

-- (((\ x. (\ y. x)) (\ a. a)) ((\x. (x x)) (\x. (x x))))
-- (λ a. a)

-- (((\x.(\y.(y x))) y) (\z.z))
-- y

-- (\x.(\y.p y)) j 1
-- (p 1)

-- (\ y. (\ x. x - y)) 5 y
-- (y-5)