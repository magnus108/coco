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
  arbitrary =
    oneof
      [ return Empty
      ]

data TkStateOne
  = One PrepaymentState
  | OneSelected PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkStateOne where
  arbitrary =
    oneof
      [ One <$> arbitrary,
        OneSelected <$> arbitrary
      ]

data TkStateTwo
  = Two PrepaymentState PrepaymentState
  | TwoOneSelected PrepaymentState PrepaymentState
  | TwoTwoSelected PrepaymentState PrepaymentState
  deriving stock (Show, Eq, Ord)

instance Arbitrary TkStateTwo where
  arbitrary =
    oneof
      [ Two <$> arbitrary <*> arbitrary,
        TwoOneSelected <$> arbitrary <*> arbitrary,
        TwoTwoSelected <$> arbitrary <*> arbitrary
      ]

clearOne :: TkStateOne -> Empty
clearOne ts = Empty

clearTwo :: TkStateTwo -> Empty
clearTwo ts = Empty

addOne :: Empty -> TkStateOne
addOne xs = One empty'

addTwo :: TkStateOne -> TkStateTwo
addTwo (One x) = Two x empty'
addTwo (OneSelected x) = TwoOneSelected x empty'

deleteTwo :: TkStateTwo -> TkStateOne
deleteTwo (Two x1 x2) = One x1
deleteTwo (TwoOneSelected x1 x2) = OneSelected x1

flipOne :: TkStateOne -> TkStateOne
flipOne (One x1) = OneSelected x1
flipOne (OneSelected x1) = One x1

selectTwoOne :: TkStateTwo -> TkStateTwo
selectTwoOne (Two x1 x2) = TwoOneSelected x1 x2
selectTwoOne (TwoOneSelected x1 x2) = Two x1 x2
selectTwoOne (TwoTwoSelected x1 x2) = TwoOneSelected x1 x2

selectTwoTwo :: TkStateTwo -> TkStateTwo
selectTwoTwo (Two x1 x2) = TwoTwoSelected x1 x2
selectTwoTwo (TwoOneSelected x1 x2) = TwoTwoSelected x1 x2
selectTwoTwo (TwoTwoSelected x1 x2) = Two x1 x2

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
    [ "clearOne" `con` (clearOne :: TkStateOne -> Empty),
      "clearTwo" `con` (clearTwo :: TkStateTwo -> Empty),
      "addOne" `con` (addOne :: Empty -> TkStateOne),
      "addTwo" `con` (addTwo :: TkStateOne -> TkStateTwo),
      "deleteTwo" `con` (deleteTwo :: TkStateTwo -> TkStateOne),
      "flipOne" `con` (flipOne :: TkStateOne -> TkStateOne),
      "selectTwoOne" `con` (selectTwoOne :: TkStateTwo -> TkStateTwo),
      "selectTwoTwo" `con` (selectTwoTwo :: TkStateTwo -> TkStateTwo),
      {-
      "editOne" `con` (editOne :: TkState -> Maybe TkState),
      "editTwo" `con` (editTwo :: TkState -> Maybe TkState),
      "cancelOne" `con` (cancelOne :: TkState -> Maybe TkState),
      "cancelTwo" `con` (cancelTwo :: TkState -> Maybe TkState),
      "confirmOne" `con` (confirmOne :: TkState -> Maybe TkState),
      "confirmTwo" `con` (confirmTwo :: TkState -> Maybe TkState),
      -}
      {-background
      [ predicate "canAddOne" (canAddOne :: TkState -> Bool),
        predicate "canAddTwo" (canAddTwo :: TkState -> Bool),
        predicate "deletable" (deletable :: TkState -> Bool),
        predicate "canSelectOne" (canSelectOne :: TkState -> Bool),
        predicate "canSelectTwo" (canSelectTwo :: TkState -> Bool),
        predicate "canDeselectOne" (canDeselectOne :: TkState -> Bool),
        predicate "canDeselectTwo" (canDeselectTwo :: TkState -> Bool),
        predicate "isJust" (isJust :: Maybe TkState -> Bool),
        predicate "==" ((==) :: TkState -> TkState -> Bool),
        predicate "/=" ((/=) :: TkState -> TkState -> Bool)
      ],
      -}
      -- monoObserve @TkState,
      mono @Empty,
      mono @TkStateOne,
      mono @TkStateTwo,
      withMaxTests 1000
    ]
