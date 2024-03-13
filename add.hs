import Test.QuickCheck

add1 :: Int -> Int
add1 x = x + 1
sub1 :: Int -> Int
sub1 x = x - 1

add_tail_recur :: Int -> Int -> Int
add_tail_recur x y
    |x == 0 =   y
    |x > 0  =   add_tail_recur (sub1 x) (add1 y)
    |x < 0  =   add_tail_recur (add1 x) (sub1 y)

add_non_tail_recur :: Int -> Int -> Int
add_non_tail_recur x y
    |x == 0 =   y
    |x > 0  =   add1 (add_non_tail_recur (sub1 x) y)
    |x < 0  =   sub1 (add_non_tail_recur (add1 x) y)

prop_add_tail_recur :: Int -> Int -> Bool
prop_add_tail_recur x y = add_tail_recur x y == (x + y)

prop_add_non_tail_recur :: Int -> Int -> Bool
prop_add_non_tail_recur x y = add_non_tail_recur x y == (x + y)

main::IO()
main = do
    quickCheck prop_add_tail_recur
    quickCheck prop_add_non_tail_recur


