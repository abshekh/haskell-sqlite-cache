module Main where

import Control.Monad (forM_, void)
import Database.SQLite.Simple (open)
import Storage.Queries.Artist
import qualified Storage.Types.InMemDb as InMemDb

main :: IO ()
main = do
  -- connInMemory <- open ":memory:"
  connInMemory <- open "file:memdb1?mode=memory&cache=shared"
  void $ InMemDb.migrateDB connInMemory
  conn <- open "resources/chinook.db"

  putStrLn "first call with id 1 ..."
  res <- selectOneArtistMaybe connInMemory conn 1

  forM_ res $ \row ->
    putStrLn ("Album: " ++ show row)

  putStrLn "second call with id 1 ..."
  res <- selectOneArtistMaybe connInMemory conn 1

  forM_ res $ \row ->
    putStrLn ("Album: " ++ show row)


  putStrLn "first call with id 2 ..."
  res <- selectOneArtistMaybe connInMemory conn 2

  forM_ res $ \row ->
    putStrLn ("Album: " ++ show row)

  putStrLn "second call with id 2 ..."
  res <- selectOneArtistMaybe connInMemory conn 2

  forM_ res $ \row ->
    putStrLn ("Album: " ++ show row)
