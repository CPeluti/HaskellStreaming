-- {-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}



module DatabaseHaspotifaskell 
    ( runDb
    , User(..), Music(..), Playlist(..), Relation, migrateAll
    , insertPlaylist, updatePlaylistName, deletePlaylist, selectPlaylistByAuthor, selectPlaylistByName, selectAllPlaylists
    , insertUser, updateUserName, updateUserEmail, updateUserPassword, deleteUser, selectAllUsers
    , insertMusic, updateMusicName, updateMusicFilePath, updateMusicAuthor, updateMusicReleaseDate, updateMusicAlbum, updateMusicFileSize, updateMusicLength, deleteMusic, selectMusicByAuthor, selectMusicByName, selectMusicByAlbum, selectMusicByRelesate, selectAllSongs
    , insertRelation, deleteRelation, selectRelationByMusic, selectRelationByPlaylist
    ) where

import            Control.Monad.Trans.Resource
import Control.Monad.Logger
-- import qualified Database.Esqueleto as E
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
-- import           Data.ByteString
import           Data.Time
import           Control.Monad.IO.Class
-- import           System.Posix.Types (UserID)
-- import Database.Persist.Sql

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    email String
    firstName String 
    lastName String
    password String
    created UTCTime default=CURRENT_TIME
    deriving Show
  Music
    filePath String
    name String
    author String
    releaseDate UTCTime default=CURRENT_TIME
    album String
    fileSize Int
    -- thumbnail ByteString
    length Int
    created UTCTime default=CURRENT_TIME
    deriving Show
  Playlist
    name String
    author UserId
    created UTCTime default=CURRENT_TIME
    deriving Show
  Relation
    playlist PlaylistId
    music MusicId
    deriving Show
|]

-- FUNCOES PLAYLIST

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT
      . runResourceT
      . withSqliteConn "dev.sqlite3"
      . runSqlConn

insertPlaylist :: (MonadIO m) => String -> Key User -> SqlPersistT m (Key Playlist)
insertPlaylist pName idUser = do
  time <- liftIO getCurrentTime
  insert $ Playlist pName idUser time

updatePlaylistName :: (MonadIO m) => Key Playlist -> String -> SqlPersistT m ()
updatePlaylistName playlistId updatedName = update playlistId [PlaylistName =. updatedName]

-- updatePlaylistImage :: (MonadIO) => Key Playlist -> ByteString -> SqlPersistT m ()

deletePlaylist :: (MonadIO m) => Key Playlist -> SqlPersistT m ()
deletePlaylist = delete

selectPlaylistByAuthor :: (MonadIO m) => Key User -> SqlPersistT m [Entity Playlist]
selectPlaylistByAuthor wantedAuthor = selectList [PlaylistAuthor ==. wantedAuthor] []

selectPlaylistByName :: (MonadIO m) => String -> SqlPersistT m [Entity Playlist]
selectPlaylistByName wantedName = selectList [PlaylistName ==. wantedName] []

selectAllPlaylists :: (MonadIO m) => SqlPersistT m [Entity Playlist]
selectAllPlaylists = do
  selectList [] [Asc PlaylistId]

-- FUNCOES USER
insertUser :: (MonadIO m) => String -> String -> String -> String -> SqlPersistT m (Key User)
insertUser uEmail uFirstName uLastName uPassword = do
  time <- liftIO getCurrentTime
  insert $ User  uEmail uFirstName uLastName uPassword time

updateUserName :: (MonadIO m) => Key User -> String -> String -> SqlPersistT m ()
updateUserName userId updatedFName updatedLName = update userId [UserFirstName =. updatedFName, UserLastName =. updatedLName]

updateUserEmail :: (MonadIO m) => Key User -> String -> SqlPersistT m ()
updateUserEmail userId updatedEmail = update userId [UserEmail =. updatedEmail]

updateUserPassword :: (MonadIO m) => Key User -> String -> SqlPersistT m ()
updateUserPassword userId updatedPassword = update userId [UserPassword =. updatedPassword]

deleteUser :: (MonadIO m) => Key User -> SqlPersistT m ()
deleteUser = delete

selectAllUsers :: (MonadIO m) => SqlPersistT m [Entity User]
selectAllUsers = do
  selectList [] [Asc UserId]

-- FUNCOES MUSICAS
insertMusic :: (MonadIO m) => String -> String -> String -> UTCTime -> String -> Int -> Int -> SqlPersistT m (Key Music)
insertMusic mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength = do
  time <- liftIO getCurrentTime
  insert $ Music mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength time

updateMusicName :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicName musicId updatedName = update musicId [MusicName =. updatedName]

updateMusicFilePath :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicFilePath musicId updatedPath = update musicId [MusicFilePath =. updatedPath]

updateMusicAuthor :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicAuthor musicId updatedAuthor = update musicId [MusicAuthor =. updatedAuthor]

updateMusicReleaseDate :: (MonadIO m) => Key Music -> UTCTime -> SqlPersistT m ()
updateMusicReleaseDate musicId updatedDate = update musicId [MusicReleaseDate =. updatedDate]

updateMusicAlbum :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicAlbum musicId updatedAlbum = update musicId [MusicAlbum =. updatedAlbum]

updateMusicFileSize :: (MonadIO m) => Key Music -> Int -> SqlPersistT m ()
updateMusicFileSize musicId updatedFileSize = update musicId [MusicFileSize =. updatedFileSize]

updateMusicLength :: (MonadIO m) => Key Music -> Int -> SqlPersistT m ()
updateMusicLength musicId updatedLength = update musicId [MusicLength =. updatedLength]

deleteMusic :: (MonadIO m) => Key Music -> SqlPersistT m ()
deleteMusic = delete

selectMusicByAuthor :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByAuthor wantedAuthor = selectList [MusicAuthor ==. wantedAuthor] []

selectMusicByName :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByName wantedName = selectList [MusicName ==. wantedName] []

selectMusicByAlbum :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByAlbum wantedAlbum = selectList [MusicAlbum ==. wantedAlbum] []

selectMusicByRelesate :: (MonadIO m) => UTCTime -> SqlPersistT m [Entity Music]
selectMusicByRelesate wantedDate = selectList [MusicReleaseDate ==. wantedDate] []

selectAllSongs :: (MonadIO m) => SqlPersistT m [Entity Music]
selectAllSongs = do
  selectList [] [Asc MusicId]

-- FUNCOES RELATION
insertRelation :: (MonadIO m) => Key Playlist -> Key Music -> SqlPersistT m (Key Relation)
insertRelation idPlaylist idMusic = do
  insert $ Relation  idPlaylist idMusic

deleteRelation :: (MonadIO m) => Key Relation -> SqlPersistT m ()
deleteRelation = delete

selectRelationByMusic :: (MonadIO m) => Key Music -> SqlPersistT m [Entity Relation]
selectRelationByMusic wantedMusic = selectList [RelationMusic ==. wantedMusic] []

selectRelationByPlaylist :: (MonadIO m) => Key Playlist -> SqlPersistT m [Entity Relation]
selectRelationByPlaylist wantedPlaylist = selectList [RelationPlaylist ==. wantedPlaylist] []
