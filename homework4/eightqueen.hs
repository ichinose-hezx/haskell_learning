queen :: Int -> [[Int]]
queen 0 = [[]]
queen n = [x:xs|xs<-queen (n-1),x<-[1..boardSize],safe (x:xs) 1]

safe ::[Int]->Int->Bool
safe [_] _ = True
safe (x:y:z) n = and [x/=y,x/=y+n,x/=y-n,safe (x:z) (n+1)]

boardSize :: Int
boardSize = 8

main :: IO()
main = do
    print $ queen boardSize
