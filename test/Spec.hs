module Main
  ( main,
  )
where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Traversable
import qualified Dejafu
import qualified Dispenser
import qualified ListZipper as LZ
import Piece (entersGroundLevel)
import Piece hiding (main)
import Relude.Unsafe ((!!))
import Test.Tasty
import qualified Test.Tasty.DejaFu as TestDejafu
import Test.Tasty.QuickCheck
import qualified WordSearch2 as WS2

-- Counting Valleys problem
genHills :: Gen [Char]
genHills = do
  listOf (elements ['U', 'D'])

valleyTests :: [Char] -> Property
valleyTests = property . maybe True (\lz -> or $ LZ.toList $ lz =>> consecutives) . parse

consecutives :: LZ.ListZipper Int -> Bool
consecutives lz = not (entersGroundLevel lz && maybe False entersGroundLevel (LZ.backward lz))

tests :: TestTree
tests =
  testGroup
    "Property Tests"
    [ -- testProperty "No consecutive valleys" (forAll genHills valleyTests),
      --     testProperty "No consecutive mountains" (forAll genHills mountainsTests),
      --      testProperty "Sequences" (forAll genSequence sequenceTests),
      testProperty "wordSearch2" (forAll dictOf wordTest)
    ]

-- Counting Mountains problem
mountainsTests :: [Char] -> Property
mountainsTests = property . maybe True (\lz -> or $ LZ.toList $ lz =>> consecutiveMountains) . parse

consecutiveMountains :: LZ.ListZipper Int -> Bool
consecutiveMountains lz = not (mountainPeeks lz && maybe False mountainPeeks (LZ.backward lz))

-- Step Sequenceing problem

genSequence :: Gen [(Sum Int, Sum Int)]
genSequence = do
  let moves = [(-1, 0), (0, -1), (1, 0), (0, 1)]
  list <- sized $ \n ->
    return $ replicate n moves
  shuffle (concat list)

sequenceTests :: [(Sum Int, Sum Int)] -> Property
sequenceTests = property . lol1 . moves

-- WordSearch2
dictOf :: Gen ([(Sum Int, Sum Int)], M.Map WS2.Coord (S.Set String), (Sum Int, Sum Int), Int, Int)
dictOf = do
  x <- chooseInt (1, 10)
  y <- chooseInt (1, 10)
  m <- fmap M.fromList $ fmap concat $ for [0 .. x - 1] $ \x' -> do
    for [0 .. y - 1] $ \y' -> do
      c <- S.singleton . (: []) <$> elements ['a' .. 'z']
      return $ ((Sum x', Sum y'), c)
  xs <- elements [0 .. (x - 1)]
  ys <- elements [0 .. (y - 1)]
  li <- chooseInt (1, 8)
  moves <- take li <$> genMoves
  return (moves, m, (Sum xs, Sum ys), x, y)

genMoves :: Gen [(Sum Int, Sum Int)]
genMoves = listOf $ elements [(-1, 0), (0, -1), (1, 0), (0, 1)]

wordTest :: ([(Sum Int, Sum Int)], M.Map WS2.Coord (S.Set String), (Sum Int, Sum Int), Int, Int) -> Property
wordTest (moves, m, start, x, y) = property $ WS2.res2 x y $ (EnvT word s)
  where
    s = WS2.startingGrid2 m start
    word = WS2.move moves "" s

--    ([(Sum {getSum = 0},Sum {getSum = 1})]
--     ,fromList [((Sum {getSum = 0},Sum {getSum = 0}),fromList ["m"]),((Sum {getSum = 0},Sum {getSum = 1}),fromList ["m"]),
--               ((Sum {getSum = 1},Sum {getSum = 0}),fromList ["m"]),((Sum {getSum = 1},Sum {getSum = 1}),fromList ["m"])]
-- ,(Sum {getSum = 1},Sum {getSum = 0})
-- ,2,2)

dispenserTest :: TestTree
dispenserTest =
  testGroup
    "Property Tests Dispenser"
    [ testProperty "Dispenser" (forAll genProgram propDispenser)
    ]

genProgram :: Gen [Char]
genProgram = do
  listOf (elements ['T', 'R'])

parseProgram :: [Char] -> IO [Int]
parseProgram xs = do
  val <- newEmptyMVar

  mapConcurrently
    ( \x -> case x of
        'I' -> Dispenser.start val
        'R' -> Dispenser.reset val
        'T' -> Dispenser.safeTakeTicket val
    )
    ('I' : xs)

propDispenser :: [Char] -> Property
propDispenser xs = ioProperty $ do
  prog <- parseProgram xs
  return $ and $ fmap (\xs -> isPrefixOf xs [0 ..]) (transpose (group (sort prog)))

dejafuTest :: TestTree
dejafuTest = TestDejafu.testAuto "put twice" Dejafu.myFunction

genDeadlockProgram :: Gen [Int]
genDeadlockProgram = do
  n <- chooseInt (0, 30)
  listOf (elements [0 .. n])

parseDeadlockProgram :: [Int] -> IO [()]
parseDeadlockProgram xs = do
  let n = 30
  mvars <- mapM (\x -> newMVar x) [0 .. n]
  mapConcurrently
    ( \x -> do
        let m1 = mvars !! (x `mod` n)
        let m2 = mvars !! ((x + 1) `mod` n)
        val1 <- takeMVar m1
        val2 <- takeMVar m2
        putMVar m2 (val1)
        putMVar m1 (val2)
    )
    xs

propDeadlock :: Int -> [Int] -> Property
propDeadlock i xs = ioProperty $ do
  asyncAction <- async (parseDeadlockProgram xs)
  result <- E.try (race (wait asyncAction) (threadDelay 6000000)) :: IO (Either SomeException (Either [()] ()))
  case result of
    Left e -> do
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      traceShowM e
      return False -- Completed within timeout
    Right x -> case x of
      Left _ -> return True -- Completed within timeout
      Right x -> do
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        traceShowM "dada"
        return False -- Blocked (didn't complete within timeout)

deadlockTest :: TestTree
deadlockTest =
  testGroup
    "Property Tests Deadlock"
    [ testProperty "Deadlock" (forAll genDeadlockProgram (propDeadlock 100000))
    ]

main :: IO ()
main =
  defaultMain $
    adjustOption (const $ QuickCheckMaxSize 1000) $
      adjustOption (const $ QuickCheckVerbose True) $
        adjustOption (const $ QuickCheckTests 10000) $
          testGroup
            "Tests"
            [ dispenserTest,
              dejafuTest,
              deadlockTest
            ]
