import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map

main :: IO ()
main = print $ runFib 7

runFib :: Int -> (Int, Cache)
runFib n = runState (fib n) []

type Cache = Map.Map Int Int

fib :: Int -> State Cache Int
fib 0 = pure 0
fib 1 = pure 1
fib n = do
  mp <- get
  return (nMinusOne + nMinusTwo)

  where nMinusOne = case Map.lookup mp (n-1) of
          Nothing -> fib (n-1)
          Just x -> x
        nMinusTwo = case Map.lookup mp (n-2) of
          Nothing -> fib (n-2)
          Just x -> x