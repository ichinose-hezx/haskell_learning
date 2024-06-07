import Control.Monad.State

type Stack = [Int]

-- 定义 pop 操作
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

-- 定义 push 操作
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

-- 定义组合操作
stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

main :: IO ()
main = do
  let initialState = [5,8,2,1]
  let (result, finalState) = runState stackManip initialState
  print result       -- 输出结果
  print finalState   -- 输出最终状态
