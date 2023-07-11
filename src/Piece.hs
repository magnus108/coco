module Piece
  ( main,
    entersGroundLevel,
    parse,
    move,
    mountainPeeks,
    isSequence,
    moves,
    lol1,
  )
where

import Control.Comonad
import Control.Comonad.Store
import qualified Control.Comonad.Traced as T
import qualified Control.Comonad.Trans.Traced as Ta
import Data.Foldable (maximum)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified ListZipper as LZ

-- Counting Valleys problem
countValleys :: String -> Int
countValleys = maybe 0 solve . parse

solve :: LZ.ListZipper Int -> Int
solve lz = lz =>> entersGroundLevel & LZ.toList & fmap fromEnum & sum

parse :: String -> Maybe (LZ.ListZipper Int)
parse = LZ.fromList >=> heights

heights :: LZ.ListZipper Char -> Maybe (LZ.ListZipper Int)
heights = foldlM (\steps movement -> LZ.forward (LZ.insert (nextStep steps movement) steps)) (LZ.singleton 0)

nextStep :: LZ.ListZipper Int -> Char -> Int
nextStep steps movement = (if up movement then (+) else (-)) (extract steps) 1

up :: Char -> Bool
up = (==) 'U'

entersGroundLevel :: LZ.ListZipper Int -> Bool
entersGroundLevel hill = extract hill == 0 && maybe False (\x -> extract x == -1) (LZ.backward hill)

-- Counting mountains problem
mountainPeeks :: LZ.ListZipper Int -> Bool
mountainPeeks hill = maybe False (\prev -> extract prev < extract hill) (LZ.backward hill) && maybe False (\next -> extract next < extract hill) (LZ.forward hill)

countMountains :: String -> Int
countMountains = maybe 0 solveMountains . parse

solveMountains :: LZ.ListZipper Int -> Int
solveMountains lz = lz =>> mountainPeeks & LZ.toList & fmap fromEnum & sum

-- Step Sequenceing problem

neighbourLocations :: (Sum Int, Sum Int) -> [(Sum Int, Sum Int)]
neighbourLocations location = mappend location <$> [(-1, 0), (0, -1), (0, 1), (1, 0)]

move :: [(Sum Int, Sum Int)] -> [(Sum Int, Sum Int)]
move movementss = traceShowId list
  where
    list = foldl' (\(y : acc) x -> (mappend y x) : y : acc) [(0, 0)] movementss

isSequence :: [(Sum Int, Sum Int)] -> Bool
isSequence xs = and $ experiment (\s -> xs) $ extend (even . sum . extract) $ extend (experiment neighbourLocations) $ store (\y -> (*) 2 $ length $ filter (\x -> x == y) xs) (0, 0)

neighbourAndSelf :: (Sum Int, Sum Int) -> [(Sum Int, Sum Int)]
neighbourAndSelf location = mappend location <$> [(-1, 0), (0, -1), (0, 0), (0, 1), (1, 0)]

len :: M.Map (Sum Int, Sum Int) [(Sum Int, Sum Int)] -> Store (Sum Int, Sum Int) [(Sum Int, Sum Int)]
len xs = store (\y -> concat $ M.lookup y xs) (0, 0)

lol1 xs = and $ experiment (const (M.keys xs)) $ extend (\ws -> even $ length $ traceShowId $ filter (\x -> x == (pos ws)) $ concat $ extract ws) $ extend (experiment neighbourAndSelf) (len xs)

moves movementss = snd $ foldl' (\(p, m) x -> ((mappend p x), M.insertWith (<>) p (S.toList (S.fromList [p, mappend p x])) m)) ((0, 0), M.empty) movementss

--- Trace

previousMonth :: T.Traced (Sum Int) Int -> Int
previousMonth = T.trace (-1)

nextMonth :: T.Traced (Sum Int) Int -> Int
nextMonth = T.trace 1

allOfThem :: T.Traced All a -> a
allOfThem = T.trace (All True)

nonOfThem :: T.Traced All a -> a
nonOfThem = T.trace (All False)

xxx :: T.Traced All Int
xxx = T.traced (\b -> if getAll b then 1 else 0)

func :: T.Traced (Sum Int) Int
func = T.traced (\m -> 5 * getSum m + 3)

func2 :: T.Traced (Sum Int) Int
func2 = T.traced (\m -> getSum m ^ 2 - 2)

-- ext = T.trace (Sum 1)

func22 :: T.Traced [Sum Int] [Int]
func22 = T.traced (fmap (T.runTraced func2))

fibo :: T.Traced (Sum Int) Int -> Int
fibo xs =
  let prev2 = T.trace (Sum (-2)) xs
      prev1 = T.trace (Sum (-1)) xs
   in T.traces (\m -> if m == 0 then Sum 0 else if m == 1 then Sum 1 else Sum (prev2 + prev1)) xs

withFibo :: T.Traced (Sum Int) (Int, Int)
withFibo = liftW2 (,) const1 (extend fibo const1)

const1 :: T.Traced (Sum Int) Int
const1 = T.traced (\x -> getSum x)

--
-- lol :: T.Traced (Sum Int) Int
-- lol = T.traced (\x -> getSum x)

-- lol2 :: T.Traced (Sum Int) Int -> Int
-- lol2 w = T.runTraced w 0
--

-- trapping rainwater
problem :: Maybe (LZ.ListZipper Int)
problem = LZ.fromList [3, 2, 0, 0, 3, 0, 2, 1, 2]

max0 :: [Int] -> Int
max0 [] = 0
max0 xs = maximum xs

waterAtPosition :: LZ.ListZipper Int -> Int
waterAtPosition (LZ.ListZipper toLeft current toRight) = max 0 (containingWallHeight - current)
  where
    containingWallHeight = min (max0 toLeft) (max0 toRight)

solution :: LZ.ListZipper Int -> Int
solution z = sum (extend waterAtPosition z)

--- HISTORY
data Dir = L | R
  deriving (Show, Eq)

-- Navigate
tZipper :: LZ.ListZipper a -> T.Traced (Dual [Dir]) a
tZipper z = T.traced (extract . follow z . getDual)
  where
    follow :: LZ.ListZipper a -> [Dir] -> LZ.ListZipper a
    follow z' [] = z'
    follow z' (L : rest) = follow (fromMaybe z' (LZ.backward z')) rest
    follow z' (R : rest) = follow (fromMaybe z' (LZ.forward z')) rest

-- GAme
data Distance = WayTooLow | TooLow | JustRight | TooHigh | WayTooHigh
  deriving (Show)

game :: Int -> T.Traced (Sum Int) Distance
game n = T.traced toDistance
  where
    toDistance (Sum guess)
      | guess > n + 5 = WayTooHigh
      | guess < n - 5 = WayTooLow
      | guess > n = TooHigh
      | guess < n = TooLow
      | otherwise = JustRight

homeIn :: Distance -> Sum Int
homeIn WayTooLow = Sum 3
homeIn TooLow = Sum 1
homeIn JustRight = Sum 0
homeIn TooHigh = Sum (-1)
homeIn WayTooHigh = Sum (-3)

main :: IO ()
main = do
  let path = "UDUDDDDDUD" -- 01010
  let valleyCount = countValleys path
  let mountainCount = countMountains path
  let pathSequence = "URDL" -- LOOP
  let gg = xxx =>> (\x -> 2 + extract x)
  print "nono"
  --  print $ T.runTraced (gg =>> nonOfThem =>> (T.traces (\a -> All (a > 2))) =>> extract . liftW2 (\x y -> x + y) xxx) (All True)
  --  print $ extract $ liftW2 (\x y -> (x == y, x, y)) func func2 =>> next =>> next =>> next
  --  print $ extract $ extend fibo $ extend fibo $ T.traced (\m -> getSum 1 + getSum 1)
  {-
  print $ T.trace (Sum 0) $ withFibo
  print $ T.trace (Sum 1) $ withFibo
  print $ T.trace (Sum 2) $ withFibo
  print $ T.trace (Sum 3) $ withFibo
  print $ T.trace (Sum 4) $ withFibo
  print $ T.trace (Sum 5) $ withFibo
  print $ T.trace (Sum 6) $ withFibo
  print $ T.trace (Sum 7) $ withFibo
  print $ T.trace (Sum 8) $ withFibo
  print $ T.trace (Sum 9) $ withFibo
  print $ T.trace (Sum 10) $ withFibo
  print $ solution <$> problem
  -}
  print $ extract $ Ta.censor (\m -> Dual (getDual m ++ [L, L])) $ extend (T.trace (Dual [R, R, R])) $ Ta.listens (id) $ tZipper $ LZ.ListZipper [] 1 [2, 3, 4, 5]

--  print $ extract $ extend (T.traces (\x -> homeIn x)) $ extend (T.traces (\x -> homeIn x)) $ game 2

--  print (lol1 (traceShowId (moves [(Sum {getSum = 0}, Sum {getSum = 1}), (Sum {getSum = 1}, Sum {getSum = 0}), (Sum {getSum = -1}, Sum {getSum = 0}), (Sum {getSum = 0}, Sum {getSum = -1})])))

--  print valleyCount
-- print mountainCount
-- print pathSequencie
