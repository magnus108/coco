module Main
  ( main,
  )
where

import Control.Comonad
import qualified ListZipper as LZ
import Piece hiding (main)
import Relude.Unsafe ((!!))
import Test.Tasty
import Test.Tasty.QuickCheck

-- Counting Valleys problem
genValleys :: Gen [Char]
genValleys = do
  listOf (elements ['U', 'D'])

prop_sortedList :: [Char] -> Property
prop_sortedList = maybe (property True) (\lz' -> conjoin $ LZ.toList $ lz' =>> entersGroundLevel =>> isPrev) . parse

isPrev :: LZ.ListZipper Bool -> Property
isPrev x = property (not (extract x && maybe False extract (LZ.backward x)))

tests :: TestTree
tests =
  testGroup
    "Property Tests"
    [ testProperty "No consecutive valleys" (forAll genValleys prop_sortedList)
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [tests]
