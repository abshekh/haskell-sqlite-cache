module Main where

import Control.Monad (forM_, void)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Storage.Queries.Artist
import qualified Storage.Types.InMemDb as InMemDb

main :: IO ()
main = do
  -- connInMemory <- SQLite.open ":memory:"
  connInMemory <- SQLite.open "file:memdb1?mode=memory&cache=shared"
  void $ InMemDb.migrateDB connInMemory
  conn <- PostgreSQL.connectPostgreSQL "host=localhost port=5432 dbname=chinook user=bilbo password=baggins"

  putStrLn "first call with id 1 ..."
  res <- selectOneArtistMaybe connInMemory conn 1

  forM_ res $ \row ->
    putStrLn ("artist: " ++ show row)

  putStrLn "second call with id 1 ..."
  res <- selectOneArtistMaybe connInMemory conn 1

  forM_ res $ \row ->
    putStrLn ("artist: " ++ show row)


  putStrLn "first call with id 2 ..."
  res <- selectOneArtistMaybe connInMemory conn 2

  forM_ res $ \row ->
    putStrLn ("artist: " ++ show row)

  putStrLn "second call with id 2 ..."
  res <- selectOneArtistMaybe connInMemory conn 2

  forM_ res $ \row ->
    putStrLn ("artist: " ++ show row)
