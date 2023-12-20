-- module Main (main) where
-- import RestApi (restApi)
-- main :: IO ()
-- main = 
--   

{-# LANGUAGE EmptyDataDecls             #-}
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
import qualified Database.Esqueleto as E
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.ByteString
import           Data.Time
import           Control.Monad.IO.Class


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
    author String
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

-- FUNCOES PLAYLIST

insertPlaylist :: (MonadIO m) => String -> String -> SqlPersistT m (Key Playlist)
insertPlaylist pName pAuthor = do
  time <- (liftIO getCurrentTime)
  id <- insert $ Playlist pName pAuthor time
  liftIO $ putStrLn $ "Playlist criada: " ++ pName
  return id

updatePlaylistName :: (MonadIO m) => Key Playlist -> String -> SqlPersistT m ()
updatePlaylistName playlistId updatedName = do
  update playlistId [PlaylistName =. updatedName]

-- updatePlaylistImage :: (MonadIO) => Key Playlist -> ByteString -> SqlPersistT m ()

deletePlaylist :: (MonadIO m) => Key Playlist -> SqlPersistT m ()
deletePlaylist playlistId = do
  delete playlistId

selectPlaylistByAuthor :: (MonadIO m) => String -> SqlPersistT m [Entity Playlist]
selectPlaylistByAuthor wantedAuthor = selectList [PlaylistAuthor ==. wantedAuthor] []

selectPlaylistByName :: (MonadIO m) => String -> SqlPersistT m [Entity Playlist]
selectPlaylistByName wantedName = selectList [PlaylistName ==. wantedName] []

-- FUNCOES USER
insertUser :: (MonadIO m) => String -> String -> String -> String -> SqlPersistT m (Key User)
insertUser uEmail uFirstName uLastName uPassword = do
  time <- (liftIO getCurrentTime)
  id <- insert $ User  uEmail uFirstName uLastName uPassword time
  return id

updateUserName :: (MonadIO m) => Key User -> String -> String -> SqlPersistT m ()
updateUserName userId updatedFName updatedLName = do
  update userId [UserFirstName =. updatedFName, UserLastName =. updatedLName]

updateUserEmail :: (MonadIO m) => Key User -> String -> SqlPersistT m ()
updateUserEmail userId updatedEmail = do
  update userId [UserEmail =. updatedEmail]

updateUserPassword :: (MonadIO m) => Key User -> String -> SqlPersistT m ()
updateUserPassword userId updatedPassword = do
  update userId [UserPassword =. updatedPassword]

deleteUser :: (MonadIO m) => Key User -> SqlPersistT m ()
deleteUser userId = do
  delete userId

-- FUNCOES MUSICAS
insertMusic :: (MonadIO m) => String -> String -> String -> UTCTime -> String -> Int -> Int -> SqlPersistT m (Key Music)
insertMusic mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength = do
  time <- (liftIO getCurrentTime)
  id <- insert $ Music mFilePath mName mAuthor mReleaseDate mAlbum mFileSize mLength time
  return id

updateMusicName :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicName musicId updatedName = do
  update musicId [MusicName =. updatedName]

updateMusicFilePath :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicFilePath musicId updatedPath = do
  update musicId [MusicFilePath =. updatedPath]

updateMusicAuthor :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicAuthor musicId updatedAuthor = do
  update musicId [MusicAuthor =. updatedAuthor]

updateMusicReleaseDate :: (MonadIO m) => Key Music -> UTCTime -> SqlPersistT m ()
updateMusicReleaseDate musicId updatedDate = do
  update musicId [MusicReleaseDate =. updatedDate]

updateMusicAlbum :: (MonadIO m) => Key Music -> String -> SqlPersistT m ()
updateMusicAlbum musicId updatedAlbum = do
  update musicId [MusicAlbum =. updatedAlbum]

updateMusicFileSize :: (MonadIO m) => Key Music -> Int -> SqlPersistT m ()
updateMusicFileSize musicId updatedFileSize = do
  update musicId [MusicFileSize =. updatedFileSize]

updateMusicLength :: (MonadIO m) => Key Music -> Int -> SqlPersistT m ()
updateMusicLength musicId updatedLength = do
  update musicId [MusicLength =. updatedLength]

deleteMusic :: (MonadIO m) => Key Music -> SqlPersistT m ()
deleteMusic musicId = do
  delete musicId

selectMusicByAuthor :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByAuthor wantedAuthor = selectList [MusicAuthor ==. wantedAuthor] []

selectMusicByName :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByName wantedName = selectList [MusicName ==. wantedName] []

selectMusicByAlbum :: (MonadIO m) => String -> SqlPersistT m [Entity Music]
selectMusicByAlbum wantedAlbum = selectList [MusicAlbum ==. wantedAlbum] []

selectMusicByRelesate :: (MonadIO m) => UTCTime -> SqlPersistT m [Entity Music]
selectMusicByRelesate wantedDate = selectList [MusicReleaseDate ==. wantedDate] []



main :: IO ()
main = runSqlite "teste.db" $ do
  runMigration migrateAll

  idPlaylist <- insertPlaylist "Musicas para dormir" "Ana"
  updatePlaylistName idPlaylist "Musicas para dancar"
  deletePlaylist idPlaylist
  
  idPlaylist1 <- insertPlaylist "Musicas legais" "Ana"
  idPlaylist2 <- insertPlaylist "Musicas tristes" "Ana"
  idPlaylist3 <- insertPlaylist "Musicas para quebrar tudo" "Ana"
  idPlaylist4 <- insertPlaylist "Musicas divertidas" "Caio"

  -- playlistsAna <- selectPlaylistByAuthor "Ana"
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist ++ " Autor: " ++ playlistAuthor playlist) playlistsAna
  
  idUser <- insertUser "teste@gmail.com" "teste" "t" "senha"
  idUser2 <- insertUser "testeteste@gmail.com" "testeteste" "tt" "senhaa"
  updateUserName idUser "teste" "tes"
  deleteUser idUser

  let releaseDate = UTCTime (fromGregorian 2018 06 21) 0
  idMusic <- insertMusic "FilePath" "musica" "autor" releaseDate "album" 20 3

  updateMusicName idMusic "novo nome"
  updateMusicAuthor idMusic "novo autor"
  updateMusicAlbum idMusic "novo album"
  updateMusicFilePath idMusic "novo path"
  let newReleaseDate = UTCTime (fromGregorian 2018 06 01) 0
  updateMusicReleaseDate idMusic newReleaseDate
  updateMusicFileSize idMusic 200
  updateMusicLength idMusic 300

  deleteMusic idMusic

  idMusic1 <- insertMusic "FilePath" "Believer" "autor1" releaseDate "album" 20 3
  idMusic2 <- insertMusic "FilePath" "Run" "autor2" releaseDate "album" 20 3
  idMusic3 <- insertMusic "FilePath" "Radioactive" "autor3" releaseDate "albumLegal" 20 3


  -- musicasLegais <- selectMusicByName "Run"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasLegais

  -- musicasAutor <- selectMusicByAuthor "autor2"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAutor

  -- musicasAlbum <- selectMusicByAlbum "album"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAlbum

  -- let queryDate = UTCTime (fromGregorian 2018 06 21) 0
  -- musicasData <- selectMusicByRelesate queryDate
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasData

  return ()