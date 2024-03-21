import Test.QuickCheck

mytwo ::Int->Int->Int
mytwo x y = if x > y then x else y

prop_Max :: Int -> Int -> Bool
prop_Max x y = (x <= =maxtwo x y) && (y <= maxtwo x y) 
main = quickCheck prop_Max