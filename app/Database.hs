{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module              Database (db) where

import              Database.SQLite.Simple
import              Database.Beam
import              Database.Beam.Migrate
import              Database.Beam.Sqlite
import              Database.Beam.Migrate.Simple (verifySchema, createSchema, VerificationResult(..))
import              Database.Beam.Sqlite.Migrate (migrationBackend)
import              Control.Exception
import              Data.ByteString
import              Data.Time.LocalTime
import              Data.Int
import              Data.Eq
import              Data.Text (Text)

db = ""

-- Generate DB
data UserT f
    = User
    { userEmail     :: Columnar f Text
    , userFirstName :: Columnar f Text
    , userLastName  :: Columnar f Text
    -- , userPhoto  :: Columnar f ByteString
    , userPassword  :: Columnar f Text }
    deriving (Generic, Beamable)
type User = UserT Identity; deriving instance Show User
data MusicT f
    = Music
    { musicFilePath     :: Columnar f Text
    , musicName :: Columnar f Text
    , musicAuthor  :: Columnar f Text
    , musicReleaseDate  :: Columnar f LocalTime
    , musicAlbum  :: Columnar f Text
    , musicFileSize  :: Columnar f Int32
    , musicThumbnail  :: Columnar f ByteString
    , musicLength  :: Columnar f Int32}
    deriving (Generic, Beamable)

data PlaylistT f
    = Playlist
    { playlistName     :: Columnar f Text
    , playlistThumbnail  :: Columnar f ByteString
    , playlistAuthor :: Columnar f Text }
    deriving (Generic, Beamable)

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = UserId . userEmail

instance Table MusicT where
    data PrimaryKey MusicT f = MusicId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = MusicId . musicName

instance Table PlaylistT where
    data PrimaryKey PlaylistT f = PlaylistId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = PlaylistId . playlistName

data AppDb f = AppDb
    { users :: f (TableEntity UserT) 
    , playlists :: f (TableEntity PlaylistT)
    , musics :: f (TableEntity MusicT) 
    } deriving (Generic, Database be)
--

appDb :: CheckedDatabaseSettings Sqlite AppDb
appDb = defaultMigratableDbSettings

initializeTables :: IO ()
initializeTables = bracket (open "haspotifaskell.db") close $ \conn ->
    runBeamSqlite conn $ verifySchema migrationBackend appDb >>= \case
        VerificationFailed _  -> createSchema migrationBackend appDb
        VerificationSucceeded -> pure ()

createUser = bracket (open "haspotifaskell.db") close $ \conn ->
    runBeamSqlite conn $ do
        insert (users appDb) $ insertValues [User "p@teste.com" "arst" "tsra" "2134"]
