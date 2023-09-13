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

addOne :: TkState -> Maybe TkState
addOne Empty = Just $ One empty'
addOne _ = Nothing

canAddOne :: TkState -> Bool
canAddOne Empty = True
canAddOne _ = False

addTwo :: TkState -> Maybe TkState
addTwo (One x) = Just $ Two x empty'
addTwo (OneSelected x) = Just $ TwoOneSelected x empty'
addTwo _ = Nothing

canAddTwo :: TkState -> Bool
canAddTwo (One x) = True
canAddTwo (OneSelected x) = True
canAddTwo _ = False

deleteTwo :: TkState -> Maybe TkState
deleteTwo (Two x1 x2) = Just $ One x1
deleteTwo (TwoOneSelected x1 x2) = Just $ OneSelected x1
deleteTwo _ = Nothing

deletable :: TkState -> Bool
deletable (Two x1 x2) = True
deletable (TwoOneSelected x1 x2) = True
deletable _ = False

selectOne :: TkState -> Maybe TkState
selectOne (One x1) = Just $ OneSelected x1
selectOne (Two x1 x2) = Just $ TwoOneSelected x1 x2
selectOne (TwoTwoSelected x1 x2) = Just $ TwoOneSelected x1 x2
selectOne _ = Nothing

canSelectOne :: TkState -> Bool
canSelectOne (One x1) = True
canSelectOne (Two x1 x2) = True
canSelectOne (TwoTwoSelected x1 x2) = True
canSelectOne _ = False

selectTwo :: TkState -> Maybe TkState
selectTwo (Two x1 x2) = Just $ TwoTwoSelected x1 x2
selectTwo (TwoOneSelected x1 x2) = Just $ TwoTwoSelected x1 x2
selectTwo _ = Nothing

canSelectTwo :: TkState -> Bool
canSelectTwo (Two x1 x2) = True
canSelectTwo (TwoOneSelected x1 x2) = False
canSelectTwo _ = False

deselectOne :: TkState -> Maybe TkState
deselectOne (OneSelected x1) = Just $ One x1
deselectOne (TwoOneSelected x1 x2) = Just $ Two x1 x2
deselectOne _ = Nothing

canDeselectOne :: TkState -> Bool
canDeselectOne (OneSelected x1) = True
canDeselectOne (TwoOneSelected x1 x2) = True
canDeselectOne _ = False

deselectTwo :: TkState -> Maybe TkState
deselectTwo (TwoTwoSelected x1 x2) = Just $ Two x1 x2
deselectTwo _ = Nothing

canDeselectTwo :: TkState -> Bool
canDeselectTwo (TwoTwoSelected x1 x2) = True
canDeselectTwo _ = False

--- Edit prepayment
editOne :: TkState -> Maybe TkState
editOne (One x1) = (\x1' -> One x1') <$> clone' x1
editOne (OneSelected x1) = (\x1' -> OneSelected x1') <$> clone' x1
editOne (Two x1 x2) = (\x1' -> Two x1' x2) <$> clone' x1
editOne (TwoOneSelected x1 x2) = (\x1' -> TwoOneSelected x1' x2) <$> clone' x1
editOne (TwoTwoSelected x1 x2) = (\x1' -> TwoTwoSelected x1' x2) <$> clone' x1
editOne _ = Nothing

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

main :: IO ()
main =
  quickSpec
    [ "clear" `con` (clear :: TkState -> TkState),
      "addOne" `con` (addOne :: TkState -> Maybe TkState),
      "addTwo" `con` (addTwo :: TkState -> Maybe TkState),
      "deleteTwo" `con` (deleteTwo :: TkState -> Maybe TkState),
      "selectOne" `con` (selectOne :: TkState -> Maybe TkState),
      "selectTwo" `con` (selectTwo :: TkState -> Maybe TkState),
      "deselectOne" `con` (deselectOne :: TkState -> Maybe TkState),
      "deselectTwo" `con` (deselectTwo :: TkState -> Maybe TkState),
      "editOne" `con` (editOne :: TkState -> Maybe TkState),
      "editTwo" `con` (editTwo :: TkState -> Maybe TkState),
      "cancelOne" `con` (cancelOne :: TkState -> Maybe TkState),
      "cancelTwo" `con` (cancelTwo :: TkState -> Maybe TkState),
      "confirmOne" `con` (confirmOne :: TkState -> Maybe TkState),
      "confirmTwo" `con` (confirmTwo :: TkState -> Maybe TkState),
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
