square :: Int -> Int
square x = x * x

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    putStrLn ("The square of " ++ show number ++ " is " ++ show (square number))