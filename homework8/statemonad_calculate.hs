import Control.Monad.State

opNum :: (Int -> Int -> Int) -> Int -> Int -> State Int Int
opNum op x y = return (op x y)

addNum :: Int -> Int -> State Int Int
addNum = opNum (+)

subNum :: Int -> Int -> State Int Int
subNum = opNum (-)

mulNum :: Int -> Int -> State Int Int
mulNum = opNum (*)

divNum :: Int -> Int -> State Int Int
divNum = opNum div

inc :: Int -> State Int Int
inc = addNum 1

dec :: Int -> State Int Int
dec = flip subNum 1

exp1 :: State Int Int
exp1 = do
  x <- get
  x <- inc x
  x <- inc x
  x <- dec x
  x <- x `addNum` 10
  x <- x `subNum` 5
  x <- x `mulNum` 6
  x `divNum` 3

main :: IO ()
main = do
  let initialState = 6
  let (result, finalState) = runState exp1 initialState
  print result       
  print finalState   