module Storage.Types.InMemDb where

import Data.String (IsString (fromString))
import Database.Beam
  ( Database,
    DatabaseSettings,
    Generic,
    TableEntity,
    TableLens (TableLens),
    dbLenses,
    dbModification,
    defaultDbSettings,
    int,
    modifyTableFields,
    nationalVarchar,
    setEntityName,
    withDbModification, maybeType,
  )
import Database.Beam.Migrate
  ( CheckedDatabaseSettings,
    Migration,
    MigrationSteps,
    createTable,
    evaluateDatabase,
    field,
    migrationStep,
    notNull,
    unCheckDatabase,
    unique,
  )
import Database.Beam.Migrate.SQL.Tables (field)
import Database.Beam.Migrate.Simple
  ( BringUpToDateHooks (runIrreversibleHook),
    bringUpToDateWithHooks,
    defaultUpToDateHooks,
  )
import Database.Beam.Sqlite (Sqlite, runBeamSqliteDebug)
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Database.SQLite.Simple (Connection)
import qualified Storage.Types.Address as Address
import qualified Storage.Types.Album as Album
import qualified Storage.Types.Artist as Artist
import qualified Storage.Types.Customer as Customer
import qualified Storage.Types.Employee as Employee
import qualified Storage.Types.Genre as Genre
import qualified Storage.Types.Invoice as Invoice
import qualified Storage.Types.InvoiceLine as InvoiceLine
import qualified Storage.Types.MediaType as MediaType
import qualified Storage.Types.Playlist as Playlist
import qualified Storage.Types.PlaylistTrack as PlaylistTrack
import qualified Storage.Types.Track as Track

data ChinookInMemDb f = ChinookInMemDb
  { _artist :: f (TableEntity Artist.ArtistT),
    _genre :: f (TableEntity Genre.GenreT),
    _mediaType :: f (TableEntity MediaType.MediaTypeT),
    _playlist :: f (TableEntity Playlist.PlaylistT)
  }
  deriving (Generic, Database be)

-- ChinookInMemDb
--   (TableLens artist)
--   (TableLens genre)
--   (TableLens mediaType)
--   (TableLens playlist) = dbLenses

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite ChinookInMemDb)
initialSetup =
  ChinookInMemDb
    <$> ( createTable "artist" $
            Artist.Artist
              { artistId = field "artistid" int notNull unique,
                artistName = field "name" (nationalVarchar (Just 120)) notNull unique
              }
        )
    <*> ( createTable "genre" $
            Genre.Genre
              { genreId = field "genreid" int notNull unique,
                genreName = field "name" (nationalVarchar (Just 120)) notNull
              }
        )
    <*> ( createTable "mediatype" $
            MediaType.MediaType
              { mediaTypeId = field "mediatypeid" int notNull unique,
                mediaTypeName = field "name" (maybeType (nationalVarchar (Just 120)))
              }
        )
    <*> ( createTable "playlist" $
            Playlist.Playlist
              { playlistId = field "playlistid" int notNull unique,
                playlistName = field "name" (maybeType (nationalVarchar (Just 120)))
              }
        )

initialSetupStep :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite ChinookInMemDb)
initialSetupStep =
  migrationStep
    "initial_setup"
    (const initialSetup)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }

migrateDB ::
  Connection ->
  IO (Maybe (CheckedDatabaseSettings Sqlite ChinookInMemDb))
migrateDB conn =
  runBeamSqliteDebug putStrLn conn $
    bringUpToDateWithHooks
      allowDestructive
      migrationBackend
      initialSetupStep

chinookInMemDb :: DatabaseSettings Sqlite ChinookInMemDb
chinookInMemDb = unCheckDatabase $ evaluateDatabase initialSetupStep
