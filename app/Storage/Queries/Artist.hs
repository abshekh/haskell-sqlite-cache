{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Queries.Artist where

import Data.Int (Int32)
import Database.Beam ((==.))
import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import Database.Beam.Sqlite (Sqlite)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import qualified Storage.Types.DB as DB
import qualified Storage.Types.InMemDb as InMemDb
import Database.Beam.Postgres (Postgres)
import Control.Monad
import Language.Haskell.TH

getDbTables ::
  ( B.DatabaseEntity Postgres DB.ChinookDb (B.TableEntity ArtistT),
    B.DatabaseEntity
      Sqlite
      InMemDb.ChinookInMemDb
      (B.TableEntity ArtistT)
  )
getDbTables = (DB._artist DB.chinookDb, InMemDb._artist InMemDb.chinookInMemDb)


queryWithFilter :: (B.BeamSqlBackend be, B.SqlEq (B.QGenExpr B.QValueContext be s) (B.Columnar f Int32), B.SqlValable (B.Columnar f Int32)) => B.HaskellLiteralForQExpr (B.Columnar f Int32) -> B.Q be db s (ArtistT f) -> B.Q be db s (ArtistT f)
queryWithFilter id' = B.filter_' (\Artist {..} -> B.sqlBool_ (artistId ==. B.val_ id'))


selectOneArtistMaybe :: SQLite.Connection -> PostgreSQL.Connection -> Int32 -> IO (Maybe (ArtistT B.Identity))
selectOneArtistMaybe connInMemory conn id' = do
  let (dbTable, dbTableInMemory) = getDbTables
      filterBy Artist {..} = B.sqlBool_ (artistId ==. B.val_ id')
      filterBy' Artist {..} = B.sqlBool_ (artistId ==. B.val_ id')
  Q.selectOneMaybeCache connInMemory conn dbTableInMemory dbTable filterBy filterBy'

      -- filterBy'' Artist {..} = B.sqlBool_ (artistId ==. B.val_ id')
      -- (filterBy, filterBy') = $(Q.filterBy (\Artist {..} -> B.sqlBool_ (artistId ==. 1)))
  -- Q.selectOneMaybeCache' connInMemory conn dbTableInMemory dbTable (queryWithFilter id')
  -- Q.selectOneMaybe conn dbTable filterBy
  -- Q.selectOneMaybeSQLite connInMemory dbTableInMemory filterBy'


