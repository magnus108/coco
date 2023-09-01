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
import Conway
import Conway (animateGrid)
import Data.Foldable (maximum)
import qualified Data.Map.Strict as M
import Data.Monoid (Product)
import qualified Data.Set as S
import qualified Deadlock
import qualified Dispenser
import qualified DistributedP
import qualified DoubleCola
import Eac
import Fibo
import qualified FileReadEmulate
import qualified Linear
import qualified ListZipper as LZ
import qualified Par
import qualified PingTc
import qualified PrintInOrder
import Relude.Unsafe (read)
import qualified Snail
import qualified Stm
import qualified WordSearch2 as WS2
import qualified Yoneda

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

homed :: T.Traced (Sum Int) (Distance, Sum Int)
homed = extend wfix (go <$> Ta.listen (game 10))
  where
    go :: (Distance, Sum Int) -> T.Traced (Sum Int) (Distance, Sum Int) -> (Distance, Sum Int)
    go a@(JustRight, _) _ = a
    go (d, x) w = trace (show x) $ T.trace (homeIn d) w

fibo :: T.Traced (Sum Int) Int -> Int
fibo xs =
  let prev2 = T.trace (Sum (-2)) xs
      prev1 = T.trace (Sum (-1)) xs
   in traceShowId $ prev1 + prev2

withFibo :: Ta.Traced (Sum Int) (Int, Int)
withFibo = liftW2 (,) const1 (extend wfix (go <$> const1))
  where
    go :: Int -> T.Traced (Sum Int) Int -> Int
    go 0 w = 0
    go 1 w = 1
    go a w =
      let prev2 = T.trace (Sum (-2)) w
          prev1 = T.trace (Sum (-1)) w
       in prev1 + prev2

const1 :: T.Traced (Sum Int) Int
const1 = T.traced (\x -> getSum x)

----INGREDIENTS
ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "sticks" = S.fromList ["wood"]
ingredientsOf "bow" = S.fromList ["sticks", "string"]
ingredientsOf "arrows" = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver" = S.fromList ["arrows", "bow"]
ingredientsOf "torches" = S.fromList ["coal", "sticks"]
ingredientsOf _ = mempty

recipes :: Store (S.Set String) (S.Set String)
recipes = store (foldMap ingredientsOf) mempty

allDeps :: Store (S.Set String) (S.Set String)
allDeps = extend wfix (go <$> recipes)
  where
    go :: S.Set String -> Store (S.Set String) (S.Set String) -> (S.Set String)
    go deps _ | S.null deps = mempty
    go deps rec = deps <> peek deps rec

-- findFirst :: Eq a => [a] -> a -> [a]
-- findFirst [] _ = []
-- findFirst (y : ys) x
-- \| x == y = [y]
--  | otherwise = findFirst ys x

-- sockMerchant
-- pairs :: Store [Int] All
-- pairs = store (\xs -> foldMap (\x -> All $ even $ length $ filter (== x) xs) xs) []

pairIt :: Int -> M.Map Int (Sum Int)
pairIt x = undefined

lol = foldl' (\acc k -> M.insertWith (<>) k (Sum 1) acc) M.empty listsocks

pairs :: Store (S.Set Int) (Maybe (Sum Int))
pairs = store (foldMap (flip M.lookup lol)) mempty

listsocks = [1, 2, 1, 2, 1, 3, 2, 3]

-- allPairs :: Store [Int] [Int]
-- allPairs = extend (\wa -> foldl' (\acc y -> _) (pos wa) (extract wa)) (pairs)

-- allPairs :: Store (S.Set Int) (M.Map Int (Sum Int))
-- allPairs = extend wfix (go <$> pairs)
-- where
--  go :: [Int] -> Store (S.Set Int) [Int] -> [Int]
-- go [] _ = []
-- go deps rec = deps <> peek deps rec

-- go deps rec = _ (pos rec {- 1,2,1 socklist-}) (extract rec {-1,1,2,2,1,1-}) -- DEP er res som skal mindskes. POS SKAL BENYTTES Gerne mere
--
-- SUPER DIGIT

superdigit :: Store [Int] Int
superdigit = store (\x -> sum x) mempty

superdigit2 :: Store [Int] Int
superdigit2 = extend wfix (go <$> superdigit)
  where
    go :: Int -> Store [Int] Int -> Int
    go x _ | x < 10 = x
    go deps rec = peek (intToList deps) rec

intToList :: Int -> [Int]
intToList n = map (\c -> read [c]) (show n)

--- RACECAR
-- Your car starts at position 0 and speed +1
-- When you get an instruction 'A', your car does the following:
-- position += speed
-- speed *= 2

start :: T.Traced (Product Int, Sum Int) (Int, Int)
start = T.traced $ \(Product x, Sum y) -> (x, y)

forward :: T.Traced (Product Int, Sum Int) (Int, Int) -> (Int, Int)
forward = T.trace (Product 2, Sum 0) . Ta.censor (\((Product x), y) -> (Product (x + 1), Sum 0))

--  let (a, b) = extract w
--     (speed, _) = T.trace (Product 2, Sum 0) w
--    (_, pos) = T.trace (Product 1, Sum a) w
-- in (speed, pos)

backward :: T.Traced (Product Int, Sum Int) (Int, Int) -> (Int, Int)
backward = extract . Ta.censor (\(Product x, Sum y) -> (traceShowId (Product x), Sum y))

-- backward :: T.Traced (Product Int, Sum Int) (Int, Int) -> (Int, Int)
-- backward w = T.traces (\(x, y) -> (traceShow x $ Product x, Sum y)) w

main :: IO ()
main = do
  {-
  print "lol"
  WS2.printGrid WS2.startingGrid
  print "lol"
  WS2.printGrid WS2.nextGrid
  print "lol"
  WS2.printGrid WS2.nextNextGrid
  print "lol"
  WS2.printGrid WS2.nextNextNextGrid
  print "lol"
  WS2.printGrid WS2.nextNextNextNextGrid
  print "lol"
  traceShowM $ WS2.res WS2.startingGrid
  print "lol"
  print $ start & extract
  print $ start =>> forward & extract
  print $ start =>> forward =>> forward & extract
  print $ start =>> forward =>> forward =>> forward & extract
  print $ start =>> forward =>> forward =>> forward =>> (\x -> backward x) =>> (\x -> backward x) =>> forward & extract
  print "SNAIL"
  -}
  -- Snail.mainer
  --  DoubleCola.mainer
  -- Dispenser.mainer
  --  Stm.mainer
  -- Linear.mainer
  --  Deadlock.mainer
  -- Par.mainer3

  --  print $ start =>> forward =>> forward =>> (extract . backward) =>> forward & extract

  -- print $ take 10 $ iterate forward2 (1, 0)

  {-
  let path = "UDUDDDDDUD" -- 01010
  let valleyCount = countValleys path
  let mountainCount = countMountains path
  let pathSequence = "URDL" -- LOOP
  let gg = xxx =>> (\x -> 2 + extract x)
  print "nono"
  print $ extract $ seek (S.fromList [1, 2, 3]) $ extend (\wa -> div (fromMaybe 0 (getSum <$> extract wa)) 2) pairs
  print $ extract $ seek [9, 8, 7, 5] $ superdigit
  print $ extract $ seek [9, 8, 7, 5] $ superdigit2
  -}

  -- DistributedP.main
  PingTc.main

-- Yoneda.mainer

--  print $ extract $ seek [1, 2, 1] $ allPairs

--  print $ T.runTraced (gg =>> nonOfThem =>> (T.traces (\a -> All (a > 2))) =>> extract . liftW2 (\x y -> x + y) xxx) (All True)
--  print $ extract $ liftW2 (\x y -> (x == y, x, y)) func func2 =>> next =>> next =>> next
--  print $ extract $ extend fibo $ extend fibo $ T.traced (\m -> getSum 1 + getSum 1)
--  print $ T.trace (Sum 0) $ withFibo
{-
print $ T.trace (Sum 2) $ withFibo
print $ T.trace (Sum 3) $ withFibo
print $ T.trace (Sum 4) $ withFibo
print $ T.trace (Sum 5) $ withFibo
print $ T.trace (Sum 6) $ withFibo
print $ T.trace (Sum 7) $ withFibo
print $ T.trace (Sum 8) $ withFibo
print $ T.trace (Sum 9) $ withFibo
print $ T.trace (Sum 10) $ withFibo
-}
{-
print $ "llol-----------"
print $ llol
print $ "llol2-----------"
print $ toStates $ nextGeneration (rule 10) (begin [False, False, True, True, False, False, True, True, False, False])
-}

-- print $ peeks id $ extend (peek (S.fromList ["string", "bow"])) $ allDeps

--  print $ T.trace 12 $ Ta.listen (game 15) =>> T.traces (homeIn . fst) =>> T.traces (homeIn . fst) =>> T.traces (homeIn . fst)
--  print $ T.trace 20 $ homed

-- print $ solution <$> problem

{-
print $ extract $ Ta.censor (\m -> Dual (getDual m ++ [L, L])) $ extend (T.trace (Dual [R, R, R])) $ Ta.listens (id) $ tZipper $ LZ.ListZipper [] 1 [2, 3, 4, 5]

-}
--  print $ extract $ extend (T.traces (\x -> homeIn x)) $ extend (T.traces (\x -> homeIn x)) $ game 2

--  print (lol1 (traceShowId (moves [(Sum {getSum = 0}, Sum {getSum = 1}), (Sum {getSum = 1}, Sum {getSum = 0}), (Sum {getSum = -1}, Sum {getSum = 0}), (Sum {getSum = 0}, Sum {getSum = -1})])))

--  print valleyCount
-- print mountainCount
-- print pathSequencie
