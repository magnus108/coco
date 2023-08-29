module Par
  ( mainer,
    mainer2,
    mainer3,
  )
where

import Control.Parallel.Strategies
import Numeric (readInt)

mainer :: IO ()
mainer = do
  let a = fib 38
  let b = fib 37
  putStrLn $ show (a, b)

mainer2 :: IO ()
mainer2 = do
  let (a, b) = runEval $ do
        a <- rpar $ fib 48
        b <- rpar $ fib 47
        rseq a
        rseq b
        return (a, b)
  putStrLn $ show (a, b)

integral :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integral f step start end = if start >= end then 0 else quad + rint
  where
    quad = (b - a) * f ((a + b) / 2)
    a = start
    b = min (start + step) end
    rint = integral f step b end

f x = sin x + cos x + sinh x + cosh x

mainer3 :: IO ()
mainer3 = putStrLn $ show $ integral f 0.000001 0 (4 * pi)

integral2 :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integral2 f step start end = foldl' (+) 0 quads
  where
    quads = map (\(a, b) -> (b - a) * f ((a + b) / 2)) steps
    steps = stepList step start end

integral3 :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integral3 f step start end = foldl' (+) 0 quads
  where
    quads = map (\(a, b) -> (b - a) * f ((a + b) / 2)) steps `using` parBuffer 100 rseq
    steps = stepList step start end

stepList :: (Ord a, Floating a) => a -> a -> a -> [(a, a)]
stepList steplength start end = if start >= end then [] else step : rest
  where
    step = (a, b)
    a = start
    b = min (start + steplength) end
    rest = stepList steplength b end

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
