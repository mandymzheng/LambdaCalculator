Lambda Calculator
For this coursework, I decided to implement a program that parses and evaluates untyped lambda expressions. The program uses alpha and beta substitution to reduce the lambda expression to its simplest form. 

All interaction with the user is performed in the command line, including inputting the expression and displaying the result of the lambda calculus 
The program can be used by running stack run:

Enter an untyped λ expression:

Please separate lambda variables with () e.g. (\x.(\y.x))x instead of (\x.\y.x)x

-- (((\x.(\y.x))(\a.a))(\b.b))

Result:

(((\x.(\y.x))(\a.a))(\b.b))

((\y.(\a.a))(\b.b))

(\a.a)

Above is an example of how the program would run if a valid lambda expression was given. The program first prints out the given expression in a formatted form, often adding more brackets to make the order of operations clear to the computer and the user. It then continues to print each step of the lambda calculus for the user to follow. 

A limitation of the program is that the input string must separate the lambda variables, or the following error will appear: 
cswk-program-exe: "<stdin>" (line 1, column 6):
unexpected '\\'
expecting end of "."

This is due to the method that the program parses lambda variables, and if they are not separated the second lambda variable would not be parsed. 

The program is also able to perform alpha substitution when needed, this is when the lambda variable has the same name as the free variable. For example, (((\x.(\y.(y x)))y) (\z.z)). Here the free variable y is the same as \y.(y x) and this can be confusing when y is beta-substituted into \x.(\y.y x) as it will result in \y.y y. It is ambiguous which is the original y that needs to be substituted and which was the free variable. Therefore, alpha substitution is needed to distinguish between the two. 

Enter an untyped λ expression:

Please separate lambda variables with () e.g. (\x.(\y.x))x instead of (\x.\y.x)x

-- (((\x.(\y.(y x)))y) (\z.z))

Result:

(((\x.(\y.(y x))) y) (\z.z))

((\!y.(!y y)) (\z.z))

((\z.z) y)

y

When the alpha substitution is performed, the lambda variable y is renamed to !y making the difference clear in the expression.
The program is also able to perform lambda expressions with numbers as free variables, and include numbers and operators in the expressions. 

Enter an untyped λ expression:

Please separate lambda variables with () e.g. (\x.(\y.x))x instead of (\x.\y.x)x

-- (\y.(\x.x-y))5 y

Result:

(((\y.(\x.(x-y))) 5) y)

((\!x.(!x-5)) y)

(y-5)


