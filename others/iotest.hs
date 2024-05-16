
-- Function to output "helloworld"
hello :: IO ()
hello = do 
    putStr "hello"
    putStrLn "world"

-- Function to read two characters from user input
getTwo :: IO (Char, Char)
getTwo = do 
    x <- getChar
    y <- getChar
    -- Clear the buffer after reading characters
    _ <- getLine
    return (x, y)

-- Function to read two lines from user input and reverse them
reverse2lines :: IO ()
reverse2lines = do
    putStrLn "Please enter the first line:"
    line1 <- getLine
    putStrLn "Please enter the second line:"
    line2 <- getLine
    let rev1 = reverse line1
    let rev2 = reverse line2
    putStrLn rev1
    putStrLn rev2

-- Main function to run all tasks
main :: IO ()
main = do
    hello
    putStrLn "hello sky"
    putStrLn "Please enter two characters:"
    chars <- getTwo
    putStrLn $ "You entered: " ++ show chars
    reverse2lines