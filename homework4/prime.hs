factor :: Int->[Int]
factor n = [m|m<-[2..n],mod n m == 0]

isprime :: Int->Bool
isprime n = [n] == factor n

numFactors :: Int -> Int
numFactors n = length (factor n)

primes :: Int->[Int]
primes n = [m|m<-[2..n],isprime m]

getpair_prime :: Int->[(Int,Int)]
getpair_prime n = [(i,j)|i<-[1..n],j<-[1..n],1<i,i<j,j<=n,isprime (i+j)]

main :: IO()
main = do
    print $ factor 833490
    print $ numFactors 833490
    print $ isprime 1009
    print $ primes 1000
    print $ getpair_prime 30
