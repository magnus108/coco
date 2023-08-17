{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

module Linear
  ( mainer,
  )
where

data Ticket = Ticket
  deriving (Show)

useTicket :: Ticket %1 -> ()
useTicket Ticket = ()

exampleUse :: Ticket %1 -> ()
exampleUse t = useTicket t

-- badExample1 :: Ticket %1 -> ((), ())
-- badExample1 t = (useTicket t, useTicket t)

-- badExample2 :: Ticket %1 -> ()
-- badExample2 t = ()

mainer2 :: IO ()
mainer2 = do
  let t = Ticket
  -- let lol = badExample1 t
  -- traceShowM lol
  putStrLn "lol"

data LockState = Acquired | Released

data Lock :: LockState -> Type where
  AcquiredLock :: Lock 'Acquired
  ReleasedLock :: Lock 'Released

acquireLock :: Lock 'Released %1 -> Lock 'Acquired
acquireLock ReleasedLock = AcquiredLock

releaseLock :: Lock 'Acquired %1 -> Lock 'Released
releaseLock AcquiredLock = ReleasedLock

releaseLock2 :: Lock 'Acquired %1 -> Lock 'Released
releaseLock2 AcquiredLock = undefined

ensureRelease :: Lock 'Acquired %1 -> (Lock 'Acquired %1 -> Lock 'Released) %1 -> Lock 'Released
ensureRelease lockA cont = cont lockA

mainer :: IO ()
mainer = do
  let lockA = acquireLock ReleasedLock
  let _ = ensureRelease lockA releaseLock2
  putStrLn "lol"
  return ()

{-

releaseLock :: Lock %1 -> IO ()
releaseLock AcquiredLock = do
    return ()

-- Example usage
main :: IO ()
main = do
    lockA <- acquireLock AcquiredLock
    releaseLock lockA
    -}
