{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SpecIt2
  ( main,
  )
where

import QuickSpec
import Test.QuickCheck

data Prepayment a where
  Normal :: Int -> Prepayment Int
  Copy :: Int -> Int -> Prepayment (Int, Int)

deriving instance Eq a => Eq (Prepayment a)

deriving instance Ord a => Ord (Prepayment a)

instance Arbitrary (Prepayment Int) where
  arbitrary = Normal <$> arbitrary

instance Arbitrary (Prepayment (Int, Int)) where
  arbitrary = do
    n1 <- arbitrary
    n2 <- arbitrary
    return $ Copy n1 n2

normal :: Int -> Prepayment Int
normal x = Normal x

-- newState :: Int -> Prepayment Int
-- newState x = Normal x

-- copyItem :: Prepayment (Int, Int)
-- copyItem = Copy 0 1

copy :: (Int -> Int) -> Prepayment Int -> Prepayment (Int, Int)
copy f (Normal x) = Copy x (f x)

confirm :: Prepayment (Int, Int) -> Prepayment Int
confirm (Copy x1 x2) = Normal x2

cancel :: Prepayment (Int, Int) -> Prepayment Int
cancel (Copy x1 x2) = Normal x1

newtype PrepaymentState = PrepaymentState Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Arbitrary)

data CloneState = CloneState Int Int
  deriving stock (Show, Eq, Ord)

instance Arbitrary CloneState where
  arbitrary = return $ CloneState 0 (0 + 1)

empty' :: PrepaymentState
empty' = PrepaymentState 0

clone' :: PrepaymentState -> CloneState
clone' (PrepaymentState x) = CloneState x (x + 1)

confirm' :: CloneState -> PrepaymentState
confirm' (CloneState x1 x2) = PrepaymentState x2

cancel' :: CloneState -> PrepaymentState
cancel' (CloneState x1 x2) = PrepaymentState x1

data Empty = Empty
  deriving stock (Show, Eq, Ord)

instance Arbitrary Empty where
  arbitrary = return Empty

addOne :: Empty -> TkStateOne
addOne xs = One empty'

newtype TkStateOne = One PrepaymentState
  deriving stock (Show, Eq, Ord)
  deriving newtype (Arbitrary)

clearOne :: TkStateOne -> Empty
clearOne ts = Empty

addTwo :: TkStateOne -> TkStateTwo
addTwo (One x) = Two x empty'

selectOne :: TkStateOne -> TkStateOneSelected
selectOne (One x1) = OneSelected x1

-- editOne :: TkStateOne -> TkStateOne
-- editOne (One x1) = (\x1' -> One x1') <$> clone' x1
-- editOne (OneSelected x1) = (\x1' -> OneSelected x1') <$> clone' x1

newtype TkStateOneSelected = OneSelected PrepaymentState
  deriving stock (Show, Eq, Ord)
  deriving newtype (Arbitrary)

clearOneSelected :: TkStateOneSelected -> Empty
clearOneSelected ts = Empty

addTwoSelected :: TkStateOneSelected -> TkStateTwoOneSelected
addTwoSelected (OneSelected x) = TwoOneSelected x empty'

deselectOne :: TkStateOneSelected -> TkStateOne
deselectOne (OneSelected x) = One x

data TkStateTwo = Two PrepaymentState PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkStateTwo where
  arbitrary = Two <$> arbitrary <*> arbitrary

clearTwo :: TkStateTwo -> Empty
clearTwo ts = Empty

deleteTwo :: TkStateTwo -> TkStateOne
deleteTwo (Two x1 x2) = One x1

selectTwoOne :: TkStateTwo -> TkStateTwoOneSelected
selectTwoOne (Two x1 x2) = TwoOneSelected x1 x2

selectTwoTwo :: TkStateTwo -> TkStateTwoTwoSelected
selectTwoTwo (Two x1 x2) = TwoTwoSelected x1 x2

data TkStateTwoOneSelected = TwoOneSelected PrepaymentState PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkStateTwoOneSelected where
  arbitrary = TwoOneSelected <$> arbitrary <*> arbitrary

clearTwoOneSelected :: TkStateTwoOneSelected -> Empty
clearTwoOneSelected ts = Empty

deselectTwoOne :: TkStateTwoOneSelected -> TkStateTwo
deselectTwoOne (TwoOneSelected x1 x2) = Two x1 x2

selectTwoTwoFromOne :: TkStateTwoOneSelected -> TkStateTwoTwoSelected
selectTwoTwoFromOne (TwoOneSelected x1 x2) = TwoTwoSelected x1 x2

deleteTwoOne :: TkStateTwoOneSelected -> TkStateOneSelected
deleteTwoOne (TwoOneSelected x1 x2) = OneSelected x1

deleteTwoOneSelected :: TkStateTwoOneSelected -> TkStateOne
deleteTwoOneSelected (TwoOneSelected x1 x2) = One x2

data TkStateTwoTwoSelected = TwoTwoSelected PrepaymentState PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkStateTwoTwoSelected where
  arbitrary = TwoTwoSelected <$> arbitrary <*> arbitrary

clearTwoTwoSelected :: TkStateTwoTwoSelected -> Empty
clearTwoTwoSelected ts = Empty

deselectTwoTwo :: TkStateTwoTwoSelected -> TkStateTwo
deselectTwoTwo (TwoTwoSelected x1 x2) = Two x1 x2

selectTwoOneFromTwo :: TkStateTwoTwoSelected -> TkStateTwoOneSelected
selectTwoOneFromTwo (TwoTwoSelected x1 x2) = TwoOneSelected x1 x2

deleteTwoTwo :: TkStateTwoTwoSelected -> TkStateOneSelected
deleteTwoTwo (TwoTwoSelected x1 x2) = OneSelected x2

deleteTwoTwoSelected :: TkStateTwoTwoSelected -> TkStateOne
deleteTwoTwoSelected (TwoTwoSelected x1 x2) = One x1

--- Edit prepayment
{-
editOne :: TkStateOne -> TkStateOne
editOne (One x1) = (\x1' -> One x1') <$> clone' x1
editOne (OneSelected x1) = (\x1' -> OneSelected x1') <$> clone' x1

editTwo :: TkState -> Maybe TkState
editTwo (Two x1 x2) = (\x2' -> Two x1 x2') <$> clone' x2
editTwo (TwoOneSelected x1 x2) = (\x2' -> TwoOneSelected x1 x2') <$> clone' x2
editTwo (TwoTwoSelected x1 x2) = (\x2' -> TwoTwoSelected x1 x2') <$> clone' x2
editTwo _ = Nothing

cancelOne :: TkState -> Maybe TkState
cancelOne (One x1) = (\x1' -> One x1') <$> cancel' x1
cancelOne (OneSelected x1) = (\x1' -> OneSelected x1') <$> cancel' x1
cancelOne (Two x1 x2) = (\x1' -> Two x1' x2) <$> cancel' x1
cancelOne (TwoOneSelected x1 x2) = (\x1' -> TwoOneSelected x1' x2) <$> cancel' x1
cancelOne (TwoTwoSelected x1 x2) = (\x1' -> TwoTwoSelected x1' x2) <$> cancel' x1
cancelOne _ = Nothing

cancelTwo :: TkState -> Maybe TkState
cancelTwo (Two x1 x2) = (\x2' -> Two x1 x2') <$> cancel' x2
cancelTwo (TwoOneSelected x1 x2) = (\x2' -> TwoOneSelected x1 x2') <$> cancel' x2
cancelTwo (TwoTwoSelected x1 x2) = (\x2' -> TwoTwoSelected x1 x2') <$> cancel' x2
cancelTwo _ = Nothing

confirmOne :: TkState -> Maybe TkState
confirmOne (One x1) = (\x1' -> One x1') <$> confirm' x1
confirmOne (OneSelected x1) = (\x1' -> OneSelected x1') <$> confirm' x1
confirmOne (Two x1 x2) = (\x1' -> Two x1' x2) <$> confirm' x1
confirmOne (TwoOneSelected x1 x2) = (\x1' -> TwoOneSelected x1' x2) <$> confirm' x1
confirmOne (TwoTwoSelected x1 x2) = (\x1' -> TwoTwoSelected x1' x2) <$> confirm' x1
confirmOne _ = Nothing

confirmTwo :: TkState -> Maybe TkState
confirmTwo (Two x1 x2) = (\x2' -> Two x1 x2') <$> confirm' x2
confirmTwo (TwoOneSelected x1 x2) = (\x2' -> TwoOneSelected x1 x2') <$> confirm' x2
confirmTwo (TwoTwoSelected x1 x2) = (\x2' -> TwoTwoSelected x1 x2') <$> confirm' x2
confirmTwo _ = Nothing
-}

main2 :: IO ()
main2 =
  quickSpec
    [ "addOne" `con` (addOne :: Empty -> TkStateOne),
      "clearOne" `con` (clearOne :: TkStateOne -> Empty),
      "addTwo" `con` (addTwo :: TkStateOne -> TkStateTwo),
      "selectOne" `con` (selectOne :: TkStateOne -> TkStateOneSelected),
      "clearOneSelected" `con` (clearOneSelected :: TkStateOneSelected -> Empty),
      "addTwoSelected" `con` (addTwoSelected :: TkStateOneSelected -> TkStateTwoOneSelected),
      "deselectOne" `con` (deselectOne :: TkStateOneSelected -> TkStateOne),
      "clearTwo" `con` (clearTwo :: TkStateTwo -> Empty),
      "deleteTwo" `con` (deleteTwo :: TkStateTwo -> TkStateOne),
      "selectTwoOne" `con` (selectTwoOne :: TkStateTwo -> TkStateTwoOneSelected),
      "selectTwoTwo" `con` (selectTwoTwo :: TkStateTwo -> TkStateTwoTwoSelected),
      "clearTwoOneSelected" `con` (clearTwoOneSelected :: TkStateTwoOneSelected -> Empty),
      "deselectTwoOne" `con` (deselectTwoOne :: TkStateTwoOneSelected -> TkStateTwo),
      "selectTwoTwoFromOne" `con` (selectTwoTwoFromOne :: TkStateTwoOneSelected -> TkStateTwoTwoSelected),
      "deleteTwoOne" `con` (deleteTwoOne :: TkStateTwoOneSelected -> TkStateOneSelected),
      "deleteTwoOneSelected" `con` (deleteTwoOneSelected :: TkStateTwoOneSelected -> TkStateOne),
      "clearTwoTwoSelected" `con` (clearTwoTwoSelected :: TkStateTwoTwoSelected -> Empty),
      "deselectTwoTwo" `con` (deselectTwoTwo :: TkStateTwoTwoSelected -> TkStateTwo),
      "selectTwoOneFromTwo" `con` (selectTwoOneFromTwo :: TkStateTwoTwoSelected -> TkStateTwoOneSelected),
      "deleteTwoTwo" `con` (deleteTwoTwo :: TkStateTwoTwoSelected -> TkStateOneSelected),
      "deleteTwoTwoSelected" `con` (deleteTwoTwoSelected :: TkStateTwoTwoSelected -> TkStateOne),
      mono @Empty,
      mono @TkStateOne,
      mono @TkStateOneSelected,
      mono @TkStateTwo,
      mono @TkStateTwoTwoSelected,
      mono @TkStateTwoOneSelected
    ]

main :: IO ()
main = do
  quickSpec
    [ "normal" `con` (normal :: Int -> Prepayment Int),
      --     "newState" `con` (newState :: Prepayment Int),
      --      "copyItem" `con` (copyItem :: Prepayment (Int, Int)),
      "copy" `con` (copy :: (Int -> Int) -> Prepayment Int -> Prepayment (Int, Int)),
      "confirm" `con` (confirm :: Prepayment (Int, Int) -> Prepayment Int),
      "cancel" `con` (cancel :: Prepayment (Int, Int) -> Prepayment Int),
      --      predicate "==" $ ((==) :: Prepayment Int -> Prepayment Int -> Bool),
      --     predicate "==" $ ((==) :: Prepayment (Int, Int) -> Prepayment (Int, Int) -> Bool),
      mono @(Prepayment Int),
      mono @(Prepayment (Int, Int)),
      background [prelude]
    ]

-- gg = normal == (cancel $ copy (\x -> x + 1) $ normal)
-- gg2 = newState == (confirm $ copy (\x -> x + 1) $ normal)
