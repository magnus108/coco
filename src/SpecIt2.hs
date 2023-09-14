{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SpecIt2
  ( main,
  )
where

import QuickSpec
import Relude.Unsafe (last)
import Test.QuickCheck
import Prelude hiding (Off, On, One, State, last)

data Prepayment a where
  Emptyy :: Prepayment ()
  Normal :: Int -> Prepayment Int
  Copy :: Int -> Int -> Prepayment (Int, Int)

deriving instance Eq a => Eq (Prepayment a)

deriving instance Ord a => Ord (Prepayment a)

instance Arbitrary (Prepayment ()) where
  arbitrary = return $ Emptyy

instance Arbitrary (Prepayment Int) where
  arbitrary = Normal <$> arbitrary

instance Arbitrary (Prepayment (Int, Int)) where
  arbitrary = Copy <$> arbitrary <*> arbitrary

emptyy :: Prepayment ()
emptyy = Emptyy

normal :: Int -> Prepayment Int
normal x = Normal x

copy :: (Int -> Int) -> Prepayment Int -> Prepayment (Int, Int)
copy f (Normal x) = Copy x (f x)

confirm :: Prepayment (Int, Int) -> Prepayment Int
confirm (Copy x1 x2) = Normal x2

cancel :: Prepayment (Int, Int) -> Prepayment Int
cancel (Copy x1 x2) = Normal x1

------------------------------------
data State = Zero | One | Two
  deriving stock (Eq, Ord, Show)

instance Arbitrary State where
  arbitrary = elements [Zero, One, Two]

data Transition a b where
  AddOne :: Transition Zero One
  AddTwo :: Transition One Two
  RemoveTwo :: Transition Two One
  RemoveOne :: Transition One Zero

instance Eq (Transition a b) where
  AddOne == AddOne = True
  AddTwo == AddTwo = True
  RemoveTwo == RemoveTwo = True
  RemoveOne == RemoveOne = True
  _ == _ = False

instance Ord (Transition a b) where
  compare AddOne AddOne = EQ
  compare AddOne _ = LT
  compare _ AddOne = GT
  compare AddTwo AddTwo = EQ
  compare AddTwo _ = LT
  compare _ AddTwo = GT
  compare RemoveTwo RemoveTwo = EQ
  compare RemoveTwo _ = LT
  compare _ RemoveTwo = GT
  compare RemoveOne RemoveOne = EQ

instance Arbitrary (Transition Zero One) where
  arbitrary = return AddOne

instance Arbitrary (Transition One Two) where
  arbitrary = return AddTwo

instance Arbitrary (Transition Two One) where
  arbitrary = return RemoveTwo

instance Arbitrary (Transition One Zero) where
  arbitrary = return RemoveOne

data StateMachine a where
  Start :: StateMachine Zero
  Start1 :: StateMachine One
  Start2 :: StateMachine Two
  Step :: StateMachine s1 -> Transition s1 s2 -> StateMachine s2

zero :: StateMachine Zero
zero = Start

addOne :: Transition Zero One -> StateMachine Zero -> StateMachine One
addOne f sm = Step sm f

removeOne :: Transition One Zero -> StateMachine One -> StateMachine Zero
removeOne f sm = Step sm f

addTwo :: Transition One Two -> StateMachine One -> StateMachine Two
addTwo f sm = Step sm f

removeTwo :: Transition Two One -> StateMachine Two -> StateMachine One
removeTwo f sm = Step sm f

-------------------------------------------------------------------------------
instance Arbitrary (StateMachine Zero) where
  arbitrary = return zero

instance Arbitrary (StateMachine One) where
  arbitrary = return Start1

instance Arbitrary (StateMachine Two) where
  arbitrary = return Start2

instance Observe State Bool (StateMachine Zero) where
  observe test machine = test == (last $ runStateMachine machine)

instance Observe State Bool (StateMachine One) where
  observe test machine = test == (last $ runStateMachine machine)

instance Observe State Bool (StateMachine Two) where
  observe test machine = test == (last $ runStateMachine machine)

runStateMachine :: StateMachine s -> [State]
runStateMachine Start = [Zero]
runStateMachine Start1 = [One]
runStateMachine Start2 = [Two]
runStateMachine (Step sm AddOne) = runStateMachine sm ++ [One]
runStateMachine (Step sm AddTwo) = [Two] ++ runStateMachine sm ++ [Two]
runStateMachine (Step sm RemoveOne) = [Zero] ++ runStateMachine sm ++ [Zero]
runStateMachine (Step sm RemoveTwo) = [One] ++ runStateMachine sm ++ [One]

main :: IO ()
main = do
  let gg = runStateMachine $ addOne AddOne $ zero
  let gg2 = runStateMachine $ removeTwo RemoveTwo $ addTwo AddTwo $ addOne AddOne $ zero
  let gg3 = (last gg) == (last gg2)
  putStrLn $ show $ last gg
  putStrLn $ show $ last gg2
  putStrLn $ show $ gg3
  quickSpec
    [ "zero" `con` (zero :: StateMachine Zero),
      "start1" `con` (Start1 :: StateMachine One),
      "start2" `con` (Start2 :: StateMachine Two),
      "addOne" `con` (addOne :: Transition Zero One -> StateMachine Zero -> StateMachine One),
      "removeOne" `con` (removeOne :: Transition One Zero -> StateMachine One -> StateMachine Zero),
      "addTwo" `con` (addTwo :: Transition One Two -> StateMachine One -> StateMachine Two),
      "removeTwo" `con` (removeTwo :: Transition Two One -> StateMachine Two -> StateMachine One),
      mono @(State),
      mono @(Transition Zero One),
      mono @(Transition One Two),
      mono @(Transition Two One),
      mono @(Transition One Zero),
      monoObserve @(StateMachine Zero) @State @Bool,
      monoObserve @(StateMachine One) @State @Bool,
      monoObserve @(StateMachine Two) @State @Bool
      --      ,background [prelude]
    ]

------------------------------------
-- data Selector a b
--  = Left' a Step
--  | Right' a b
--  | None' a b

-- data Tk a b where
-- Empti :: Tk (Prepayment ()) (Prepayment ())
-- One :: Prepayment a -> Tk (Prepayment a) (Prepayment ())
-- OneSelected :: Prepayment a -> Tk (Prepayment a) (Prepayment ())
-- Two :: Prepayment a -> Prepayment b -> Tk (Prepayment a) (Prepayment b)
-- TwoOneSelected :: Prepayment a -> Prepayment b -> Tk (Prepayment a) (Prepayment b)
-- TwoTwoSelected :: Prepayment a -> Prepayment b -> Tk (Prepayment a) (Prepayment b)

------------------------------------

-- empti :: Tk (Prepayment ()) (Prepayment ())
-- empti = Empti

-- addOne :: Empty -> TkStateOne
-- addOne xs = One empty'

{-
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
    -}

main2 :: IO ()
main2 = do
  quickSpec
    [ "emptyy" `con` (emptyy :: Prepayment ()),
      "normal" `con` (normal :: Int -> Prepayment Int),
      --     "newState" `con` (newState :: Prepayment Int),
      --      "copyItem" `con` (copyItem :: Prepayment (Int, Int)),
      "copy" `con` (copy :: (Int -> Int) -> Prepayment Int -> Prepayment (Int, Int)),
      "confirm" `con` (confirm :: Prepayment (Int, Int) -> Prepayment Int),
      "cancel" `con` (cancel :: Prepayment (Int, Int) -> Prepayment Int),
      --      predicate "==" $ ((==) :: Prepayment Int -> Prepayment Int -> Bool),
      --     predicate "==" $ ((==) :: Prepayment (Int, Int) -> Prepayment (Int, Int) -> Bool),
      mono @(Prepayment ()),
      mono @(Prepayment Int),
      mono @(Prepayment (Int, Int)),
      background [prelude]
    ]

-- gg = normal == (cancel $ copy (\x -> x + 1) $ normal)
-- gg2 = newState == (confirm $ copy (\x -> x + 1) $ normal)
