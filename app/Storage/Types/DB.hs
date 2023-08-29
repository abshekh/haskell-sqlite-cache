module Storage.Types.DB where

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
    withDbModification,
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

data ChinookDb entity = ChinookDb
  { _album :: entity (TableEntity Album.AlbumT),
    _artist :: entity (TableEntity Artist.ArtistT),
    _customer :: entity (TableEntity Customer.CustomerT),
    _employee :: entity (TableEntity Employee.EmployeeT),
    _genre :: entity (TableEntity Genre.GenreT),
    _invoice :: entity (TableEntity Invoice.InvoiceT),
    _invoiceLine :: entity (TableEntity InvoiceLine.InvoiceLineT),
    _mediaType :: entity (TableEntity MediaType.MediaTypeT),
    _playlist :: entity (TableEntity Playlist.PlaylistT),
    _playlistTrack :: entity (TableEntity PlaylistTrack.PlaylistTrackT),
    _track :: entity (TableEntity Track.TrackT)
  }
  deriving (Generic)

instance Database be ChinookDb

addressFields b =
  Address.Address
    (fromString (b <> "Address"))
    (fromString (b <> "City"))
    (fromString (b <> "State"))
    (fromString (b <> "Country"))
    (fromString (b <> "PostalCode"))

chinookDb :: DatabaseSettings be ChinookDb
chinookDb =
  defaultDbSettings
    `withDbModification` ( dbModification
                             { _album =
                                 setEntityName "album"
                                   <> modifyTableFields (Album.Album "albumid" "title" (Artist.ArtistId "artistid")),
                               _artist =
                                 setEntityName "artist"
                                   <> modifyTableFields (Artist.Artist "artistid" "name"),
                               _customer =
                                 setEntityName "customer"
                                   <> modifyTableFields
                                     ( Customer.Customer
                                         "customerid"
                                         "firstname"
                                         "lastname"
                                         "company"
                                         (addressFields "")
                                         "phone"
                                         "fax"
                                         "email"
                                         (Employee.EmployeeId "supportrepid")
                                     ),
                               _employee =
                                 setEntityName "employee"
                                   <> modifyTableFields
                                     ( Employee.Employee
                                         "employeeid"
                                         "lastname"
                                         "firstname"
                                         "title"
                                         (Employee.EmployeeId "reportsto")
                                         "birthdate"
                                         "hiredate"
                                         (addressFields "")
                                         "phone"
                                         "fax"
                                         "email"
                                     ),
                               _genre =
                                 setEntityName "genre"
                                   <> modifyTableFields
                                     (Genre.Genre "genreid" "name"),
                               _invoice =
                                 setEntityName "invoice"
                                   <> modifyTableFields
                                     ( Invoice.Invoice
                                         "invoiceid"
                                         (Customer.CustomerId "customerid")
                                         "invoicedate"
                                         (addressFields "billing")
                                         "total"
                                     ),
                               _invoiceLine =
                                 setEntityName "invoiceline"
                                   <> modifyTableFields
                                     ( InvoiceLine.InvoiceLine
                                         "invoicelineid"
                                         (Invoice.InvoiceId "invoiceid")
                                         (Track.TrackId "trackid")
                                         "unitprice"
                                         "quantity"
                                     ),
                               _mediaType =
                                 setEntityName "mediatype"
                                   <> modifyTableFields (MediaType.MediaType "mediatypeid" "name"),
                               _playlist =
                                 setEntityName "playlist"
                                   <> modifyTableFields (Playlist.Playlist "playlistid" "name"),
                               _playlistTrack =
                                 setEntityName "playlisttrack"
                                   <> modifyTableFields
                                     ( PlaylistTrack.PlaylistTrack
                                         (Playlist.PlaylistId "playlistid")
                                         (Track.TrackId "trackid")
                                     ),
                               _track =
                                 setEntityName "track"
                                   <> modifyTableFields
                                     ( Track.Track
                                         "trackid"
                                         "name"
                                         (Album.AlbumId "albumid")
                                         (MediaType.MediaTypeId "mediatypeid")
                                         (Genre.GenreId "genreid")
                                         "composer"
                                         "milliseconds"
                                         "bytes"
                                         "unitprice"
                                     )
                             }
                         )

-- ChinookDb
--   (TableLens album)
--   (TableLens artist)
--   (TableLens customer)
--   (TableLens employee)
--   (TableLens genre)
--   (TableLens invoice)
--   (TableLens invoiceLine)
--   (TableLens mediaType)
--   (TableLens playlist)
--   (TableLens playlistTrack)
--   (TableLens track) = dbLenses

