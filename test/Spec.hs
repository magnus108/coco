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
genValleys :: Gen [Char]
genValleys = do
  listOf (elements ['U', 'D'])

valleyTests :: [Char] -> Property
valleyTests = maybe (property True) (\lz -> conjoin $ LZ.toList $ lz =>> consecutives) . parse

consecutives :: LZ.ListZipper Int -> Property
consecutives lz = property (not (entersGroundLevel lz && maybe False entersGroundLevel (LZ.backward lz)))

tests :: TestTree
tests =
  testGroup
    "Property Tests"
    [ testProperty "No consecutive valleys" (forAll genValleys valleyTests)
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [tests]
