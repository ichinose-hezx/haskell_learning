import Data.List

takeWord :: String -> String
takeWord [] = []
takeWord (x:xs) 
    | notSpace x = x:takeWord xs
    | otherwise = []
    where
        notSpace x = not $ elem x ", .\n"

dropWord :: String -> String
dropWord xs = drop (length(takeWord xs)) xs

takeWords :: String -> [String]
takeWords [] = []
takeWords (x:xs) 
    | notSpace x = takeWord (x:xs):takeWords (dropWord (x:xs))
    | otherwise = takeWords xs
    where
        notSpace x = not $ elem x ", .\n"

countWords :: [[String]] -> [(String,Int)]
countWords = map (\wds -> (head wds,length wds))

format :: [(String,Int)] ->[String]
format = map (\(wds,cts) -> wds++":"++(show cts))

count_all = unlines.format.countWords.group.sort.takeWords

main :: IO()
main = do
    putStr $ count_all "hello clouds \n hello sky"
