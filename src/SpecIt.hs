{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SpecIt
  ( main,
  )
where

import qualified Data.Map as M
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Poly (OrdA)
import Prelude hiding (empty)

newtype Trie a = Trie (M.Map a (Trie a))
  deriving stock (Show, Eq, Ord)
  deriving newtype (Typeable)

instance (Arbitrary a, Ord a) => Arbitrary (Trie a) where
  arbitrary = do
    words' <- arbitrary
    return $ trie words'

instance (Ord a) => Semigroup (Trie a) where
  (Trie m1) <> (Trie m2) = Trie (M.unionWith (<>) m1 m2)

empty :: Trie a
empty = Trie M.empty

unTrie :: Trie a -> M.Map a (Trie a)
unTrie (Trie m) = m

trie :: Ord a => [[a]] -> Trie a
trie = foldl' insert empty

insert :: Ord a => Trie a -> [a] -> Trie a
insert t [] = t
insert (Trie m) (x : xs) = Trie $ M.unionWith (<>) (M.singleton x (insert empty xs)) m

main :: IO ()
main =
  quickSpec
    [ "empty" `con` (empty :: Trie A),
      "unTrie" `con` (unTrie :: Trie A -> M.Map A (Trie A)),
      "trie" `con` (trie :: [[OrdA]] -> Trie OrdA),
      "insert" `con` (insert :: Trie OrdA -> [OrdA] -> Trie OrdA),
      monoType (Proxy :: Proxy OrdA),
      inst (Sub Dict :: (Ord A) :- Ord (Trie A)),
      inst (Sub Dict :: (Ord A, Arbitrary A) :- Arbitrary (Trie A)),
      inst (Sub Dict :: (Ord A, Ord B) :- Ord (M.Map A B)),
      inst (Sub Dict :: (Ord A, Ord B, Arbitrary A, Arbitrary B) :- Arbitrary (M.Map A B)),
      background
        [ lists,
          predicate "member" (M.member :: OrdA -> M.Map OrdA B -> Bool),
          con "empty" (M.empty :: M.Map A B),
          con "singleton" (M.singleton :: A -> B -> M.Map A B),
          con "delete" (M.delete :: OrdA -> M.Map OrdA B -> M.Map OrdA B),
          con "fromlist" (M.fromList :: [(OrdA, B)] -> M.Map OrdA B),
          con "toList" (M.toList :: M.Map A B -> [(A, B)])
        ]
    ]
