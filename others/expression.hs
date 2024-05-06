data Expr = Num Int|Var String|Add Expr Expr|Mul Expr Expr
    deriving (Show)

eval :: Expr -> Int
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

differentiate :: Expr -> String -> Expr
differentiate (Num n) _ = Num 0
differentiate (Var y) x 
    |x==y = Num 1
    |otherwise = Num 0
differentiate (Add x y) z = Add (differentiate x z) (differentiate y z)
differentiate (Mul x y) z = Add (Mul x (differentiate y z)) (Mul y (differentiate x z))

main :: IO()
main = do
    print $ eval (Mul (Add (Num 1) (Num 2)) (Num 3))
    print $ differentiate (Mul (Num 2) (Var "x")) "x"