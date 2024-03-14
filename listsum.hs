import Test.QuickCheck

listsum_non_tail :: [Int] -> Int
listsum_non_tail [] = 0
listsum_non_tail (x:xs) = listsum_non_tail (xs) + x

listsum_tail :: [Int] -> Int -> Int
listsum_tail [] y = y
listsum_tail (x:xs) y = listsum_tail xs (x+y)

prop_listsum_non_tail :: [Int] -> Bool
prop_listsum_non_tail xs = listsum_non_tail xs == sum xs

prop_listsum_tail :: [Int] -> Bool
prop_listsum_tail xs = listsum_tail xs 0 == sum xs
 
main ::IO()
main = do
    quickCheck prop_listsum_non_tail
    quickCheck prop_listsum_tail
    