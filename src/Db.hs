{-# LANGUAGE OverloadedStrings #-}

module Db
  ( main,
  )
where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data User = User Int String
  deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field

main :: IO ()
main = do
  conn <- open "mydatabase.db"

  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"

  -- Insert some users
  let users = [("John Doe"), ("Jane Doe"), ("Mike Jordan")] :: [String]

  forM_ users $ \name ->
    execute conn "INSERT INTO users (name) VALUES (?)" (Only name)

  rows <- query_ conn "SELECT id, name FROM users" :: IO [User]
  mapM_ print rows
  close conn
