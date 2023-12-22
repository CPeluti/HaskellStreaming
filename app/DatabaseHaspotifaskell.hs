{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}



module DatabaseHaspotifaskell where

import            Control.Monad.Trans.Resource
import Control.Monad.Logger
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time
import           Data.Int
import           Control.Monad.IO.Class
import           Models


-- FUNCOES PLAYLIST

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT
      . runResourceT
      . withSqliteConn "dev.sqlite3"
      . runSqlConn

insertPlaylist :: (MonadIO m) => String -> Key User -> SqlPersistT m (Key Playlist)
insertPlaylist pName idUser = do
  insert $ Playlist pName idUser

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

selectPlaylistById :: (MonadIO m) => Key Playlist -> SqlPersistT m (Maybe Playlist)
selectPlaylistById = get

selectPlaylistByIdInt :: (MonadIO m) => Int64 -> SqlPersistT m (Maybe Playlist)
selectPlaylistByIdInt idValue = get $ toSqlKey idValue

-- FUNCOES USER
insertUser :: (MonadIO m) => String -> String -> String -> String -> SqlPersistT m (Key User)
insertUser uEmail uFirstName uLastName uPassword = do
  insert $ User  uEmail uFirstName uLastName uPassword

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

selectUserById :: (MonadIO m) => Key User -> SqlPersistT m (Maybe User)
selectUserById = get

selectUserByIdInt :: (MonadIO m) => Int64 -> SqlPersistT m (Maybe User)
selectUserByIdInt idValue = get $ toSqlKey idValue

-- FUNCOES MUSICAS
insertMusic :: (MonadIO m) => String -> String -> String -> UTCTime -> String -> Int -> Int -> SqlPersistT m (Key Music)
insertMusic mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength = do
  insert $ Music mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength

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

selectMusicById :: (MonadIO m) => Key Music -> SqlPersistT m (Maybe Music)
selectMusicById = get

selectMusicByIdInt :: (MonadIO m) => Int64 -> SqlPersistT m (Maybe Music)
selectMusicByIdInt idValue = get $ toSqlKey idValue

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
