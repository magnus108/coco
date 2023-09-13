{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SpecIt2
  ( main,
  )
where

import QuickSpec
import Test.QuickCheck

data PrepaymentState
  = PrepaymentState Int
  | CloneState Int Int
  deriving stock (Show, Eq, Ord)

instance Arbitrary PrepaymentState where
  arbitrary = oneof [return $ PrepaymentState 0, return $ CloneState 0 (0 + 1)]

empty' :: PrepaymentState
empty' = PrepaymentState 0

clone' :: PrepaymentState -> Maybe PrepaymentState
clone' (PrepaymentState x) = Just $ CloneState x (x + 1)
clone' _ = Nothing

confirm' :: PrepaymentState -> Maybe PrepaymentState
confirm' (CloneState x1 x2) = Just $ PrepaymentState x2
confirm' (PrepaymentState x) = Nothing

cancel' :: PrepaymentState -> Maybe PrepaymentState
cancel' (CloneState x1 x2) = Just $ PrepaymentState x1
cancel' (PrepaymentState x) = Nothing

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

main :: IO ()
main =
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
      mono @TkStateTwoOneSelected,
      withMaxTests 1000
    ]
