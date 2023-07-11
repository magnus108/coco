module Main
  ( main,
  )
where

import Control.Comonad
import qualified ListZipper as LZ
import Piece (entersGroundLevel)
import Piece hiding (main)
import Relude.Unsafe ((!!))
import Test.Tasty
import Test.Tasty.QuickCheck

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
    [ testProperty "No consecutive valleys" (forAll genHills valleyTests),
      testProperty "No consecutive mountains" (forAll genHills mountainsTests),
      testProperty "Sequences" (forAll genSequence sequenceTests)
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

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [tests]
