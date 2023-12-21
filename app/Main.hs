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
module Main (main) where
import RestApi (restApi)
import qualified Database.Esqueleto as E
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.ByteString
import           Data.Time
import           Control.Monad.IO.Class
import           DatabaseHaspotifaskell
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)

createDummyData :: SqlPersistT (NoLoggingT (ResourceT IO)) ([Playlist], [Music])
createDummyData = do
    currentTime <- liftIO getCurrentTime
    mockUserId <- insertUser "example@email.com" "FirstName" "LastName" "passwordHash"
    let dummyPlaylists = [Playlist "Playlist 1" mockUserId currentTime,
                          Playlist "Playlist 2" mockUserId currentTime,
                          Playlist "Playlist 3" mockUserId currentTime]
    let dummyTracks = [Music "path/to/track1.mp3" "Track 1" "Artist 1" currentTime "Album 1" 320000 180 currentTime,
                       Music "path/to/track2.mp3" "Track 2" "Artist 2" currentTime "Album 2" 320000 180 currentTime,
                       Music "path/to/track3.mp3" "Track 3" "Artist 3" currentTime "Album 3" 320000 180 currentTime]
    return (dummyPlaylists, dummyTracks)

main :: IO ()
main = runSqlite "teste.db" $ do
  runMigration migrateAll

  (playlists, tracks) <- createDummyData
  liftIO $ restApi playlists tracks

  idUser <- insertUser "teste@gmail.com" "teste" "t" "senha"
  idUser2 <- insertUser "testeteste@gmail.com" "testeteste" "tt" "senhaa"
  updateUserName idUser "teste" "tes"
  -- deleteUser idUser
  -- users <- selectAllUsers
  -- liftIO $ mapM_ (\(Entity _ user) -> putStrLn $ "Nome: " ++ userFirstName user) users



  idPlaylist <- insertPlaylist "Musicas para dormir" idUser2
  updatePlaylistName idPlaylist "Musicas para dancar"
  deletePlaylist idPlaylist
  
  idPlaylist1 <- insertPlaylist "Musicas legais" idUser2
  idPlaylist2 <- insertPlaylist "Musicas tristes" idUser2
  idPlaylist3 <- insertPlaylist "Musicas para quebrar tudo" idUser2
  idPlaylist4 <- insertPlaylist "Musicas divertidas" idUser2

  -- playlists <- selectAllPlaylists
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist) playlists

  -- playlistsAna <- selectPlaylistByAuthor "Ana"
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist ++ " Autor: " ++ playlistAuthor playlist) playlistsAna

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

  songs <- selectAllSongs
  liftIO $ mapM_ (\(Entity _ song) -> putStrLn $ "Nome: " ++ musicName song) songs

  -- musicasLegais <- selectMusicByName "Run"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasLegais

  -- musicasAutor <- selectMusicByAuthor "autor2"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAutor

  -- musicasAlbum <- selectMusicByAlbum "album"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAlbum

  -- let queryDate = UTCTime (fromGregorian 2018 06 21) 0
  -- musicasData <- selectMusicByRelesate queryDate
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasData

  idRelation <- insertRelation idPlaylist1 idMusic2
  idRelation2 <- insertRelation idPlaylist1 idMusic2
  idRelation3 <- insertRelation idPlaylist1 idMusic2
  deleteRelation idRelation3

  idRelation4 <- insertRelation idPlaylist1 idMusic1
  idRelation5 <- insertRelation idPlaylist1 idMusic3
  idRelation6 <- insertRelation idPlaylist2 idMusic3
  idRelation7 <- insertRelation idPlaylist2 idMusic1

  -- musicasPlaylist1 <- selectRelationByPlaylist idPlaylist1
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Musica: " ++ show (relationMusic relation)) musicasPlaylist1

  -- musicasPlaylist2 <- selectRelationByPlaylist idPlaylist2
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Musica: " ++ show (relationMusic relation)) musicasPlaylist2

  -- inWhatPlaylistMusic1 <- selectRelationByMusic idMusic3
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Playlist: " ++ show (relationPlaylist relation)) inWhatPlaylistMusic1

  return ()
