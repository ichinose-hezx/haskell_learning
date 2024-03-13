import Test.QuickCheck

max3 :: Int -> Int -> Int -> Int
max3 x y z = if (x >= y) && (x >= z) 
                then x 
                else if (y >= x) && (y >= z) 
                    then y 
                    else z

prop_Max3 :: Int -> Int -> Int -> Bool
prop_Max3 x y z = (x <= max3 x y z) && (y <= max3 x y z) && (z <= max3 x y z)
                     && (x == max3 x y z || y == max3 x y z || z == max3 x y z )
main = do
  let args = stdArgs { maxSuccess = 9999 }
  quickCheckWith args prop_Max3