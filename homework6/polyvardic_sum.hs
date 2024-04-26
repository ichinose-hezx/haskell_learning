--{-# LANGUAGE FlexibleInstances #-}
class R r where
    sumpoly :: Integer -> r

instance R Integer where
    sumpoly x = x

instance (Integral a,R r)=>R (a->r) where
    sumpoly x y = sumpoly (x + toInteger y)

main :: IO ()
main = do
    print $ (sumpoly 1 2 3 4 5::Integer)
    print $ (sumpoly 12::Integer)
