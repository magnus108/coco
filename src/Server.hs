{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server
  ( main,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (unwords, words)
import Network
import Relude.Unsafe (read)
import System.IO
import Text.Printf
import Prelude hiding (atomically, hSetBuffering, newTVarIO, unwords, words)

{-

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" (host :: String) (show port :: String)
    forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444

-- >>

-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String

data Client = Client
  { clientName :: ClientName,
    clientHandle :: Handle,
    clientKicked :: TVar (Maybe String),
    clientSendChan :: TChan Message
  }

-- >>

-- <<newClient
newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return
    Client
      { clientName = name,
        clientHandle = handle,
        clientSendChan = c,
        clientKicked = k
      }

-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server {clients = c}

-- >>

-- <<Message
data Message
  = Notice String
  | Tell ClientName String
  | Broadcast ClientName String
  | Command String

-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast Server {..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)

-- >>

-- <<sendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client {..} msg =
  writeTChan clientSendChan msg

-- >>

-- <<sendToName
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server {..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing -> return False
    Just client -> sendMessage client msg >> return True

-- >>

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server {..} Client {..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
    then return ()
    else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server {..} who by = do
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)

-- -----------------------------------------------------------------------------
-- The main server

talk :: Handle -> Server -> IO ()
talk handle server@Server {..} = do
  hSetNewlineMode handle universalNewlineMode
  -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
  where
    -- <<readName
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
        then readName
        else mask $ \restore -> do
          -- <1>
          ok <- checkAddClient server name handle
          case ok of
            Nothing -> restore $ do
              -- <2>
              hPrintf
                handle
                "The name %s is in use, please choose another\n"
                name
              readName
            Just client ->
              restore (runClient server client) -- <3>
                `finally` removeClient server name

-- >>

-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server {..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do
      client <- newClient name handle
      writeTVar clients $ Map.insert name client clientmap
      broadcast server $ Notice (name ++ " has connected")
      return (Just client)

-- >>

-- <<removeClient
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server {..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server {..} client@Client {..} = do
  race server receive
  return ()
  where
    receive = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage client (Command msg)

    server = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason ->
          return $
            hPutStrLn clientHandle $
              "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          io <- handleMessage serv client msg
          return $ do
            continue <- io
            when continue $ server

-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> STM (IO Bool)
handleMessage server client@Client {..} message =
  case message of
    Notice msg -> output $ "*** " ++ msg
    Tell name msg -> output $ "*" ++ name ++ "*: " ++ msg
    Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
    Command msg ->
      case words msg of
        ["/kick", who] -> do
          kick server who clientName
          return $ return True
        "/tell" : who : what -> do
          return $ do
            tell server client who (unwords what)
            return True
        ["/quit"] ->
          return $ return False
        ('/' : _) : _ -> return $ do
          hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
          return True
        _ -> do
          broadcast server $ Broadcast clientName msg
          return $ return True
  where
    output s = return $ do hPutStrLn clientHandle s; return True

-}

talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan
  race (server h factor c) (receive h c)
  return ()

receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line

server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor
  hPrintf h "current factor: $d\n" f
  loop f
  where
    loop f = do
      action <- atomically $ do
        f' <- readTVar factor
        if (f /= f')
          then return (newfactor f')
          else do
            l <- readTChan c
            return (command f l)
      action

    newfactor f = do
      hPrintf h "new factor : $d\n" f
      loop f

    command f s = case s of
      "end" -> hPutStrLn h "Thank you for using the Haskell doubling service."
      ('*' : s) -> do
        atomically $ writeTVar factor (read s :: Integer)
        loop f
      line -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f

port :: Int
port = 8988

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  factor <- atomically $ newTVar 2
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" (host :: String) (show port :: String)
    forkFinally (talk handle factor) (\_ -> hClose handle)
