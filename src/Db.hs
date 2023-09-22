{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data User = User Int String
  deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field

main :: IO ()
main = do
  conn <- open "mydatabase.db"
  rows <- query_ conn "SELECT id, name FROM users" :: IO [User]
  mapM_ print rows
  close conn
