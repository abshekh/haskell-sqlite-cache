{-# LANGUAGE RankNTypes #-}

module Storage.Queries.Artist where

import Data.Int (Int32)
import Database.Beam ((==.))
import qualified Database.Beam as B
import Database.Beam.Sqlite (Sqlite)
import Database.SQLite.Simple (Connection)
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import qualified Storage.Types.DB as DB
import qualified Storage.Types.InMemDb as InMemDb

getDbTables ::
  ( B.DatabaseEntity be DB.ChinookDb (B.TableEntity ArtistT),
    B.DatabaseEntity
      Sqlite
      InMemDb.ChinookInMemDb
      (B.TableEntity ArtistT)
  )
getDbTables = (DB._artist DB.chinookDb, InMemDb._artist InMemDb.chinookInMemDb)

selectOneArtistMaybe :: Connection -> Connection -> Int32 -> IO (Maybe (ArtistT B.Identity))
selectOneArtistMaybe connInMemory conn id' = do
  let (dbTable, dbTableInMemory) = getDbTables
      filterBy Artist {..} = B.sqlBool_ (artistId ==. B.val_ id')
  Q.selectOneMaybeCache connInMemory conn dbTable dbTableInMemory filterBy
