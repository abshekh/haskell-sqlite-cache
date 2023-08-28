module Storage.Queries.DBQueries where

import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Database.Beam.Schema.Tables (HasConstraint, FieldsFulfillConstraint)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)

seleteOneMaybe ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Sqlite
    -- B.BeamSqlBackend be,
    -- B.MonadBeam be SqliteM,
    -- B.HasQBuilder be,
    -- B.Projectible be (table B.Identity)
  ) =>
  Connection ->
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  IO (Maybe (table B.Identity))
seleteOneMaybe conn dbTable predicate = do
  runBeamSqliteDebug putStrLn conn $
    B.runSelectReturningOne $
      B.select $
        B.filter_' predicate $
          B.all_ dbTable

selectOneMaybeCache ::
  ( B.Beamable table,
    B.Generic (table B.Identity),
    B.Generic (table B.Exposed),
    B.FromBackendRow be (table B.Identity),
    B.Database be db1,
    B.Database be db2,
    be ~ Sqlite,
    (B.Generic (table (HasConstraint (B.HasSqlValueSyntax SqliteValueSyntax)))),
    FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize be) table
  ) =>
  Connection ->
  Connection ->
  B.DatabaseEntity be db1 (B.TableEntity table) ->
  B.DatabaseEntity be db2 (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  IO (Maybe (table B.Identity))
selectOneMaybeCache connInMemory conn dbTableInMemory dbTable filterBy = do
  res <- seleteOneMaybe connInMemory dbTableInMemory filterBy
  case res of
    Just _ -> do
      putStrLn "Found in in-memory cache"
      return res
    Nothing -> do
      putStrLn "Not found in in-memory cache, querying db"
      res' <- seleteOneMaybe conn dbTable filterBy
      case res' of
        Just r -> do
          putStrLn "Inserting in in-memory cache"
          runBeamSqliteDebug putStrLn connInMemory $
            B.runInsert $ B.insert dbTableInMemory $ B.insertValues [r]
          return res'
        Nothing -> return Nothing
