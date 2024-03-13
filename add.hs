add1 :: Int -> Int
add1 x = x + 1
sub1 :: Int -> Int
sub1 x = x - 1

add :: Int -> Int -> Int
add x y
    |x == 0 =   y
    |x /= 0 =   add (sub1 x) (add1 y)


main :: IO()
main = print (add 3 5)
