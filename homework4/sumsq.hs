f1 :: Int->[Int]
f1 0 = []
f1 n = [1..n]

f2 :: [Int]->[Int]
f2 xs = [x^2|x<-xs]

f3 :: [Int]->Int
f3 xs = sum xs

f :: Int->Int
f = f3.f2.f1

main ::IO()
main = do
    print $ f 5