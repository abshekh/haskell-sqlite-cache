{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Queries.DBQueries where

import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import qualified Database.Beam.Query.Internal as B
import Database.Beam.Schema.Tables (FieldsFulfillConstraint, HasConstraint)
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.SQLite.Simple as SQLite
import Data.IORef

selectOneMaybe ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  PostgreSQL.Connection ->
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  IO (Maybe (table B.Identity))
selectOneMaybe conn dbTable predicate = do
  runBeamPostgresDebug putStrLn conn $
    B.runSelectReturningOne $
      B.select $
        B.filter_' predicate $
          B.all_ dbTable

selectOneMaybeSQLite ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Sqlite
  ) =>
  SQLite.Connection ->
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  -- (forall be'. (table (B.QExpr be' B.QBaseScope) -> B.QExpr be' B.QBaseScope B.SqlBool)) ->
  -- (forall be. (table (B.QGenExpr B.QValueContext be B.QBaseScope) -> B.QGenExpr B.QValueContext be B.QBaseScope B.SqlBool)) ->
  IO (Maybe (table B.Identity))
selectOneMaybeSQLite conn dbTable predicate = do
  runBeamSqliteDebug putStrLn conn $
    B.runSelectReturningOne $
      B.select $
        B.filter_' predicate $
          B.all_ dbTable

selectOneMaybeCache ::
  ( B.Beamable table,
    B.Database Sqlite db1,
    B.Database Postgres db2,
    B.FromBackendRow Sqlite (table B.Identity),
    B.FromBackendRow Postgres (table B.Identity),
    B.Generic (table (HasConstraint (B.HasSqlValueSyntax SqliteValueSyntax))),
    B.Generic (table B.Identity),
    B.Generic (table B.Exposed),
    (B.Generic (table (HasConstraint (B.HasSqlValueSyntax SqliteValueSyntax)))),
    FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize Sqlite) table
  ) =>
  SQLite.Connection ->
  PostgreSQL.Connection ->
  B.DatabaseEntity Sqlite db1 (B.TableEntity table) ->
  B.DatabaseEntity Postgres db2 (B.TableEntity table) ->
  -- (forall be. (B.BeamSqlBackend be, B.BeamBackend be) => (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool)) ->
  (table (B.QExpr Sqlite B.QBaseScope) -> B.QExpr Sqlite B.QBaseScope B.SqlBool) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope B.SqlBool) ->
  IO (Maybe (table B.Identity))
selectOneMaybeCache connInMemory conn dbTableInMemory dbTable filterBy filterBy' = do
  res <- selectOneMaybeSQLite connInMemory dbTableInMemory filterBy
  case res of
    Just _ -> do
      putStrLn "Found in in-memory cache"
      return res
    Nothing -> do
      putStrLn "Not found in in-memory cache, querying db"
      res' <- selectOneMaybe conn dbTable filterBy'
      case res' of
        Just r -> do
          putStrLn "Inserting in in-memory cache"
          runBeamSqliteDebug putStrLn connInMemory $
            B.runInsert $
              B.insert dbTableInMemory $
                B.insertValues [r]
          return res'
        Nothing -> return Nothing


