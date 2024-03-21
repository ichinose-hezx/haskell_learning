{-# OPTIONS_GHC -O2 -rtsopts -with-rtsopts=-K32M #-}
{-# LANGUAGE BangPatterns #-}
sum2::(Num a)=>[a]->a->a
sum2 [] !temp=temp
sum2 (x:xs) !temp=sum2 xs (x+temp)
sum1::(Num a)=>[a]->a
sum1 (x:xs)=x+sum1 xs
sum1 []=0
main = print (sum2 [1..10000000] 0 )
--main = print (sum1 [1..1000000] )