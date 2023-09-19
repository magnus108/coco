{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SpecIt2
  ( main,
  )
where

import Debug.Breakpoint
import QuickSpec
import Relude.Unsafe (last)
import Test.QuickCheck
import Text.Show (Show (..), show)
import Prelude hiding (Off, On, One, Show, State, last, mzero, show)

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
{-data State = Zero | One | Two
  deriving stock (Eq, Ord, Show)

instance Arbitrary State where
  arbitrary = elements [Zero, One, Two]

data Transition :: State -> State -> Type where
  AddOne :: Transition 'Zero 'One
  AddTwo :: Transition 'One 'Two
  RemoveTwo :: Transition 'Two 'One
  RemoveOne :: Transition 'One 'Zero

data StateMachine :: State -> Type where
  MZero :: StateMachine 'Zero
  MOne :: StateMachine 'One
  MTwo :: StateMachine 'Two
  Step :: StateMachine s1 -> Transition s1 s2 -> StateMachine s2

mzero :: StateMachine 'Zero
mzero = MZero

mone :: StateMachine 'One
mone = MOne

mtwo :: StateMachine 'Two
mtwo = MTwo

addOne :: StateMachine 'Zero -> StateMachine 'One
addOne sm = Step sm AddOne

removeOne :: StateMachine 'One -> StateMachine 'Zero
removeOne sm = Step sm RemoveOne

addTwo :: StateMachine 'One -> StateMachine 'Two
addTwo sm = Step sm AddTwo

removeTwo :: StateMachine 'Two -> StateMachine 'One
removeTwo sm = Step sm RemoveTwo

runStateMachine2 :: StateMachine a -> O
runStateMachine2 MZero = O MZero
runStateMachine2 MOne = O MOne
runStateMachine2 MTwo = O MTwo
runStateMachine2 (Step sm AddOne) = runStateMachine2 sm
runStateMachine2 (Step sm AddTwo) = runStateMachine2 sm
runStateMachine2 (Step sm RemoveOne) = runStateMachine2 sm
runStateMachine2 (Step sm RemoveTwo) = runStateMachine2 sm

data Either3 a b c
  = In1 a
  | In2 b
  | In3 c
  deriving (Eq, Ord, Show)

runStateMachine3 :: O -> Either3 (StateMachine 'Zero) (StateMachine 'One) (StateMachine 'Two)
runStateMachine3 x = case x of
  (O s@MZero) -> In1 s
  (O s@MOne) -> In2 s
  (O s@MTwo) -> In3 s

runStateMachine :: StateMachine a -> State
runStateMachine MZero = Zero
runStateMachine MOne = One
runStateMachine MTwo = Two
runStateMachine (Step sm AddOne) = runStateMachine sm
runStateMachine (Step sm AddTwo) = runStateMachine sm
runStateMachine (Step sm RemoveOne) = runStateMachine sm
runStateMachine (Step sm RemoveTwo) = runStateMachine sm

-- Define a class that represents something that can be converted to State
class AsState a where
  toState :: a -> State

-- Define A to hold any type that can be converted to State
data O = forall a. AsState a => O a

-- Make StateMachine a instance of AsState
instance AsState (StateMachine a) where
  toState = runStateMachine

-- Make A an instance of AsState by unwrapping it
instance AsState O where
  toState (O a) = toState a

instance Eq O where
  (O a1) == (O a2) = toState a1 == toState a2

instance Ord O where
  compare (O a1) (O a2) = compare (toState a1) (toState a2)

runAnyStateMachine :: O -> State
runAnyStateMachine = toState

instance Arbitrary O where
  arbitrary =
    oneof
      [ return (O MZero),
        return (O MOne),
        return (O MTwo)
      ]

instance Arbitrary (StateMachine 'Zero) where
  arbitrary = return MZero

instance Arbitrary (StateMachine 'One) where
  arbitrary = return MOne

instance Arbitrary (StateMachine 'Two) where
  arbitrary = return MTwo

instance Eq (StateMachine a) where
  sm1 == sm2 = runStateMachine sm1 == runStateMachine sm2

instance Ord (StateMachine a) where
  compare sm1 sm2 = compare (runStateMachine sm1) (runStateMachine sm2)

-- runStateMachineA :: StateMachine AnyState -> State
-- runStateMachineA (

-- data AnyStateMachine = forall a. AnyStateMachine (StateMachine a)
-- runAnyStateMachine :: AnyStateMachine -> State
-- runAnyStateMachine (AnyStateMachine sm) = runStateMachine sm

-- toAnyStateMachine :: StateMachine a -> AnyStateMachine
-- toAnyStateMachine = AnyStateMachine

-- instance Arbitrary AnyStateMachine where
-- arbitrary =
--   oneof
--   [ return $ AnyStateMachine MZero,
--   return $ AnyStateMachine MOne,
-- return $ AnyStateMachine MTwo,
-- do
-- (AnyStateMachine sm) <- arbitrary
-- case sm of
-- MZero -> return $ AnyStateMachine (Step sm AddOne)
-- MOne -> oneof [return $ AnyStateMachine (Step sm AddTwo), return $ AnyStateMachine (Step sm RemoveOne)]
-- MTwo -> return $ AnyStateMachine (Step sm RemoveTwo)
-- ]

main :: IO ()
main = do
  putStrLn "lol"
  quickSpec
    [ -- con "runStateMachine" $ (runAnyStateMachine :: O -> State),
      --      con "A" (O :: StateMachine 'Zero -> O),
      --     con "A" (O :: StateMachine 'One -> O),
      --    con "A" (O :: StateMachine 'Two -> O),
      con "mzero" (mzero :: StateMachine 'Zero),
      con "mone" (mone :: StateMachine 'One),
      con "mtwo" (mtwo :: StateMachine 'Two),
      con "addOne" (addOne :: StateMachine 'Zero -> StateMachine 'One),
      con "removeOne" (removeOne :: StateMachine 'One -> StateMachine 'Zero),
      con "addTwo" (addTwo :: StateMachine 'One -> StateMachine 'Two),
      con "removeTwo" (removeTwo :: StateMachine 'Two -> StateMachine 'One),
      --    mono @(O),
      --   mono @(State),
      mono @(StateMachine 'Zero),
      mono @(StateMachine 'One),
      mono @(StateMachine 'Two)
    ]

-}
-- quickSpec
-- []

------------------------------------
-- data Selector a b
--  = Left' a b
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
  breakpointIO
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

data Symbol = Zero | One
  deriving stock (Eq, Ord, Show)

instance Arbitrary Symbol where
  arbitrary = elements [Zero, One]

data StateMachine (s :: Symbol) where
  ZeroState :: StateMachine 'Zero
  OneState :: StateMachine 'One
  Step :: StateMachine s1 -> Transition s1 s2 -> StateMachine s2

instance Show (StateMachine a) where
  show sm = show $ runStateMachine sm

instance Eq (StateMachine a) where
  sm1 == sm2 = runStateMachine sm1 == runStateMachine sm2

instance Ord (StateMachine a) where
  compare sm1 sm2 = compare (runStateMachine sm1) (runStateMachine sm2)

instance Arbitrary (StateMachine 'Zero) where
  arbitrary = return ZeroState

instance Arbitrary (StateMachine 'One) where
  arbitrary = return OneState

data HiddenStateMachine = forall s. Hidden (StateMachine s)

instance Eq HiddenStateMachine where
  a == b = reveal a == reveal b

instance Ord HiddenStateMachine where
  compare a b = compare (reveal a) (reveal b)

instance Arbitrary HiddenStateMachine where
  arbitrary = elements [hide ZeroState, hide OneState]

hide :: StateMachine s -> HiddenStateMachine
hide = Hidden

elim :: (forall a. Ord a => a -> r) -> HiddenStateMachine -> r
elim f (Hidden s) = f s

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show (HasShow s) = show s

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

lol :: forall a. Show a => a -> HasShow
lol x = HasShow x

tt :: HasShow -> String
tt = elimHasShow (show)

instance Arbitrary HasShow where
  arbitrary = elements [lol mzero, lol mone, lol $ addOne mzero, lol $ removeOne mone]

instance Eq HasShow where
  sm1 == sm2 = tt sm1 == tt sm2

instance Ord HasShow where
  compare a b = compare (tt a) (tt b)

-- gg :: HiddenStateMachine -> Symbol
-- gg s = elim (runStateMachine) s

reveal :: HiddenStateMachine -> Either (StateMachine 'Zero) (StateMachine 'One)
reveal (Hidden s) = case runStateMachine s of
  a@(Zero) -> Left ZeroState
  a@(One) -> Right OneState

data Transition :: Symbol -> Symbol -> Type where
  AddOne :: Transition 'Zero 'One
  RemoveOne :: Transition 'One 'Zero

mzero :: StateMachine 'Zero
mzero = ZeroState

mone :: StateMachine 'One
mone = OneState

addOne :: StateMachine 'Zero -> StateMachine 'One
addOne sm = Step sm AddOne

removeOne :: StateMachine 'One -> StateMachine 'Zero
removeOne sm = Step sm RemoveOne

runStateMachine :: StateMachine a -> Symbol
runStateMachine ZeroState = Zero
runStateMachine OneState = One
runStateMachine (Step sm AddOne) = runStateMachine sm
runStateMachine (Step sm RemoveOne) = runStateMachine sm

main :: IO ()
main = do
  putStrLn "lol"
  quickSpec
    [ con "mzero" (mzero :: StateMachine 'Zero),
      con "mone" (mone :: StateMachine 'One),
      con "addOne" (addOne :: StateMachine 'Zero -> StateMachine 'One),
      con "removeOne" (removeOne :: StateMachine 'One -> StateMachine 'Zero),
      con "elim" $ (tt :: HasShow -> String),
      con "lol" $ liftC @(Show A) $ (lol :: A -> HasShow),
      mono @(HasShow),
      mono @(StateMachine 'Zero),
      mono @(StateMachine 'One)
    ]
