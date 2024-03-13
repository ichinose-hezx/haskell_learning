listsum :: [Int] -> Int
listsum [] = 0
listsum (x:xs) = sum(xs) + x

main = print( listsum [1,2,3] )