{-# LANGUAGE GADTs #-}

module Account
  ( main,
  )
where

import Control.Monad.Operational
import qualified Data.Map as Map
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Poly (OrdA)

data AccountInstruction account a where
  CreateAccount :: Int -> AccountInstruction account account
  Deposit :: account -> Int -> AccountInstruction account ()
  Withdraw :: account -> Int -> AccountInstruction account (Either TransactionError ())
  GetBalance :: account -> AccountInstruction account Int

type AccountProgram account a = Program (AccountInstruction account) a

createAccountP :: AccountProgram account account
createAccountP = singleton (CreateAccount 0)

depositP :: account -> Int -> AccountProgram account ()
depositP account amount = singleton (Deposit account amount)

withdrawP :: account -> Int -> AccountProgram account (Either TransactionError ())
withdrawP account amount = singleton (Withdraw account amount)

getBalanceP :: account -> AccountProgram account Int
getBalanceP account = singleton (GetBalance account)

data PureAccount = PureAccount
  { purebalance :: Int
  }
  deriving (Eq, Ord, Show)

instance Arbitrary PureAccount where
  arbitrary = PureAccount <$> arbitrary

type PureState = Map.Map PureAccount Int

-- The evaluation function for the pure interpreter
evalPure :: AccountProgram PureAccount a -> PureState -> a
evalPure = eval . view
  where
    eval :: ProgramView (AccountInstruction PureAccount) a -> (PureState -> a)
    eval (Return x) = const x
    eval (CreateAccount n :>>= k) = \s ->
      let newAccount = PureAccount (Map.size s + 1)
          newState = Map.insert newAccount n s
       in evalPure (k newAccount) newState
    eval (Deposit acc n :>>= k) = \s ->
      let newState = Map.adjust (+ n) acc s
       in evalPure (k ()) newState
    eval (Withdraw acc n :>>= k) = \s ->
      let balance = Map.findWithDefault 0 acc s
          result = if balance < n then Left InsufficientFunds else Right ()
          newState = if result == Right () then Map.adjust (subtract n) acc s else s
       in evalPure (k result) newState
    eval (GetBalance acc :>>= k) = \s ->
      let balance = Map.findWithDefault 0 acc s
       in evalPure (k balance) s

data TransactionError = InsufficientFunds
  deriving (Eq, Ord, Show)

data Account = Account
  { balance :: TVar Int
  }

createAccount :: Int -> STM Account
createAccount initialBalance = do
  balance <- newTVar initialBalance
  return $ Account balance

deposit :: Account -> Int -> STM ()
deposit account amt = do
  modifyTVar' (balance account) (+ amt)

withdraw :: Account -> Int -> STM (Either TransactionError ())
withdraw account amt = do
  bal <- readTVar (balance account)
  if bal < amt
    then return $ Left InsufficientFunds
    else do
      modifyTVar' (balance account) (subtract amt)
      return $ Right ()

getBalance :: Account -> STM Int
getBalance account = do
  readTVar (balance account)

--
-- Combine them into a single program
simpleProgram :: AccountProgram account Int
simpleProgram = do
  newAccount <- createAccountP
  depositP newAccount 20
  withdrawResult <- withdrawP newAccount 25
  finalBalance <- getBalanceP newAccount
  return finalBalance

main :: IO ()
main = do
  let initialState = Map.empty
  let finalBalance = evalPure simpleProgram initialState
  putStrLn $ "Final Balance: " ++ show finalBalance
  quickSpec
    [ con "createAccountP" (createAccountP :: AccountProgram PureAccount PureAccount),
      con "depositP" (depositP :: PureAccount -> Int -> AccountProgram PureAccount ()),
      con "withdrawP" (withdrawP :: PureAccount -> Int -> AccountProgram PureAccount (Either TransactionError ())),
      con "getBalanceP" (getBalanceP :: PureAccount -> AccountProgram PureAccount Int),
      con ">>=" ((>>=) :: AccountProgram PureAccount A -> (A -> AccountProgram PureAccount B) -> AccountProgram PureAccount B),
      con "evalPure" (evalPure :: AccountProgram PureAccount A -> PureState -> A),
      con "initialState" (Map.empty :: PureState) -- ,
      --   mono @(A),
      --    mono @(PureState),
      --     mono @(PureAccount),
      --      monoObserve @(AccountProgram PureAccount A)
    ]

-- transfer :: Account -> Account -> Int -> STM (Either TransactionError ())
-- transfer srcAcc destAcc amt = do
--  res <- withdraw srcAcc amt
-- case res of
--  Left err -> return $ Left err
-- Right () -> do
-- deposit destAcc amt
--  return $ Right ()

-- main2 :: IO ()
-- main2 = do
-- acc1 <- atomically $ createAccount 1000
-- acc2 <- atomically $ createAccount 500
-- acc3 <- atomically $ createAccount 700

-- let transaction = [Deposit acc1 200, Withdraw acc1 100, Withdraw acc1 50, Deposit acc1 200] -- Transfer 300]
-- atomically $ do
-- mapM_ interpret transaction

-- atomically $ do
-- _ <- deposit acc1 200
-- _ <- withdraw acc2 100
-- _ <- transfer acc1 acc3 300
-- return ()

-- Print account summaries
-- accounts <- return [acc1, acc2, acc3]
--  forM_ accounts printAccountSummary
