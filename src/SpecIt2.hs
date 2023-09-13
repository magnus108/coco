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
  arbitrary = do
    n <- arbitrary
    oneof [return $ PrepaymentState n, return $ CloneState n (n + 1)]

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

data TkState
  = Empty
  | One PrepaymentState
  | OneSelected PrepaymentState
  | Two PrepaymentState PrepaymentState
  | TwoOneSelected PrepaymentState PrepaymentState
  | TwoTwoSelected PrepaymentState PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkState where
  arbitrary =
    oneof
      [ return Empty,
        One <$> arbitrary,
        OneSelected <$> arbitrary,
        Two <$> arbitrary <*> arbitrary,
        TwoOneSelected <$> arbitrary <*> arbitrary,
        TwoTwoSelected <$> arbitrary <*> arbitrary
      ]

clear :: TkState -> TkState
clear ts = Empty

addOne :: TkState -> TkState
addOne Empty = One empty'
addOne xs = xs

canAddOne :: TkState -> Bool
canAddOne Empty = True
canAddOne _ = False

addTwo :: TkState -> TkState
addTwo (One x) = Two x empty'
addTwo (OneSelected x) = TwoOneSelected x empty'
addTwo xs = xs

canAddTwo :: TkState -> Bool
canAddTwo (One x) = True
canAddTwo (OneSelected x) = True
canAddTwo _ = False

deleteTwo :: TkState -> TkState
deleteTwo (Two x1 x2) = One x1
deleteTwo (TwoOneSelected x1 x2) = OneSelected x1
deleteTwo xs = xs

deletable :: TkState -> Bool
deletable (Two x1 x2) = True
deletable (TwoOneSelected x1 x2) = True
deletable _ = False

selectOne :: TkState -> TkState
selectOne (One x1) = OneSelected x1
selectOne (Two x1 x2) = TwoOneSelected x1 x2
selectOne (TwoTwoSelected x1 x2) = TwoOneSelected x1 x2
selectOne xs = xs

canSelectOne :: TkState -> Bool
canSelectOne (One x1) = True
canSelectOne (Two x1 x2) = True
canSelectOne (TwoTwoSelected x1 x2) = True
canSelectOne _ = False

selectTwo :: TkState -> TkState
selectTwo (Two x1 x2) = TwoTwoSelected x1 x2
selectTwo (TwoOneSelected x1 x2) = TwoTwoSelected x1 x2
selectTwo xs = xs

canSelectTwo :: TkState -> Bool
canSelectTwo (Two x1 x2) = True
canSelectTwo (TwoOneSelected x1 x2) = False
canSelectTwo _ = False

deselectOne :: TkState -> TkState
deselectOne (OneSelected x1) = One x1
deselectOne (TwoOneSelected x1 x2) = Two x1 x2
deselectOne xs = xs

canDeselectOne :: TkState -> Bool
canDeselectOne (OneSelected x1) = True
canDeselectOne (TwoOneSelected x1 x2) = True
canDeselectOne _ = False

deselectTwo :: TkState -> TkState
deselectTwo (TwoTwoSelected x1 x2) = Two x1 x2
deselectTwo xs = xs

canDeselectTwo :: TkState -> Bool
canDeselectTwo (TwoTwoSelected x1 x2) = True
canDeselectTwo _ = False

--- Edit prepayment
editOne :: TkState -> TkState
editOne g@(One x1) = fromMaybe g $ (\x1' -> One x1') <$> clone' x1
editOne g@(OneSelected x1) = fromMaybe g $ (\x1' -> OneSelected x1') <$> clone' x1
editOne g@(Two x1 x2) = fromMaybe g $ (\x1' -> Two x1' x2) <$> clone' x1
editOne g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x1' -> TwoOneSelected x1' x2) <$> clone' x1
editOne g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x1' -> TwoTwoSelected x1' x2) <$> clone' x1
editOne xs = xs

editTwo :: TkState -> TkState
editTwo g@(Two x1 x2) = fromMaybe g $ (\x2' -> Two x1 x2') <$> clone' x2
editTwo g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x2' -> TwoOneSelected x1 x2') <$> clone' x2
editTwo g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x2' -> TwoTwoSelected x1 x2') <$> clone' x2
editTwo xs = xs

cancelOne :: TkState -> TkState
cancelOne g@(One x1) = fromMaybe g $ (\x1' -> One x1') <$> cancel' x1
cancelOne g@(OneSelected x1) = fromMaybe g $ (\x1' -> OneSelected x1') <$> cancel' x1
cancelOne g@(Two x1 x2) = fromMaybe g $ (\x1' -> Two x1' x2) <$> cancel' x1
cancelOne g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x1' -> TwoOneSelected x1' x2) <$> cancel' x1
cancelOne g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x1' -> TwoTwoSelected x1' x2) <$> cancel' x1
cancelOne xs = xs

cancelTwo :: TkState -> TkState
cancelTwo g@(Two x1 x2) = fromMaybe g $ (\x2' -> Two x1 x2') <$> cancel' x2
cancelTwo g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x2' -> TwoOneSelected x1 x2') <$> cancel' x2
cancelTwo g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x2' -> TwoTwoSelected x1 x2') <$> cancel' x2
cancelTwo xs = xs

confirmOne :: TkState -> TkState
confirmOne g@(One x1) = fromMaybe g $ (\x1' -> One x1') <$> confirm' x1
confirmOne g@(OneSelected x1) = fromMaybe g $ (\x1' -> OneSelected x1') <$> confirm' x1
confirmOne g@(Two x1 x2) = fromMaybe g $ (\x1' -> Two x1' x2) <$> confirm' x1
confirmOne g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x1' -> TwoOneSelected x1' x2) <$> confirm' x1
confirmOne g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x1' -> TwoTwoSelected x1' x2) <$> confirm' x1
confirmOne xs = xs

confirmTwo :: TkState -> TkState
confirmTwo g@(Two x1 x2) = fromMaybe g $ (\x2' -> Two x1 x2') <$> confirm' x2
confirmTwo g@(TwoOneSelected x1 x2) = fromMaybe g $ (\x2' -> TwoOneSelected x1 x2') <$> confirm' x2
confirmTwo g@(TwoTwoSelected x1 x2) = fromMaybe g $ (\x2' -> TwoTwoSelected x1 x2') <$> confirm' x2
confirmTwo xs = xs

main :: IO ()
main =
  quickSpec
    [ "clear" `con` (clear :: TkState -> TkState),
      "addOne" `con` (addOne :: TkState -> TkState),
      "addTwo" `con` (addTwo :: TkState -> TkState),
      "deleteTwo" `con` (deleteTwo :: TkState -> TkState),
      "selectOne" `con` (selectOne :: TkState -> TkState),
      "selectTwo" `con` (selectTwo :: TkState -> TkState),
      "deselectOne" `con` (deselectOne :: TkState -> TkState),
      "deselectTwo" `con` (deselectTwo :: TkState -> TkState),
      "editOne" `con` (editOne :: TkState -> TkState),
      "editTwo" `con` (editTwo :: TkState -> TkState),
      "cancelOne" `con` (cancelOne :: TkState -> TkState),
      "cancelTwo" `con` (cancelTwo :: TkState -> TkState),
      "confirmOne" `con` (confirmOne :: TkState -> TkState),
      "confirmTwo" `con` (confirmTwo :: TkState -> TkState),
      background
        [ predicate "canAddOne" (canAddOne :: TkState -> Bool),
          predicate "canAddTwo" (canAddTwo :: TkState -> Bool),
          predicate "deletable" (deletable :: TkState -> Bool),
          predicate "canSelectOne" (canSelectOne :: TkState -> Bool),
          predicate "canSelectTwo" (canSelectTwo :: TkState -> Bool),
          predicate "canDeselectOne" (canDeselectOne :: TkState -> Bool),
          predicate "canDeselectTwo" (canDeselectTwo :: TkState -> Bool)
        ],
      mono @TkState,
      mono @PrepaymentState,
      withMaxTests 1000
    ]
