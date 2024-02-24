import Control.Monad.State
import Control.Monad.Identity

main :: IO ()
main = print $ runFib 7

runFib :: Int -> (Int, Cache)
runFib n = runState (fib n) []

type Cache = [(Int, Int)]

fib :: Int -> State Cache Int
fib 0 = pure 0
fib 1 = pure 1
fib n = StateT $ \s ->
    case lookup n s of
      Just v -> Identity (v, s)
      Nothing -> let (r1, s1) = runState (fib (n - 1)) s
                     (r2, s2) = runState (fib (n - 2)) s1
                     r = r1 + r2
                 in Identity (r, (n, r) : s2)