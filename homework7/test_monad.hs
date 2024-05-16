import Control.Monad

example :: Maybe Int
example = Just 3 >>= (\x -> Just (x + 2)) >>= (\y -> Just (y * 2))

f :: Int -> Maybe String
f x = if x > 0 then Just (show x) else Nothing

g :: String -> Maybe Int
g s = if length s > 2 then Just (length s) else Nothing

h :: Int -> Maybe Int
h = f >=> g

main :: IO()
main = do
    print $ example
    print $ h 556