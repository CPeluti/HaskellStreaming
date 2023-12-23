{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Models
import Control.Monad.IO.Class
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
-- import Data.ByteString
import Data.Time
-- import qualified Database.Esqueleto as E
-- import Database.Persist
import Database.Persist.Sqlite
-- import Database.Persist.TH
import DatabaseHaspotifaskell
import RestApi (restApi)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.ByteString
import           Data.Time
import           Control.Monad.IO.Class
import           DatabaseHaspotifaskell

import Models

createDummyData :: SqlPersistT (ResourceT (NoLoggingT IO)) ([Playlist], [Music])
createDummyData = do
  currentTime <- liftIO getCurrentTime
  mockUserId <- insertUser "example@email.com" "FirstName" "LastName" "passwordHash"
  let dummyPlaylists =
        [ Playlist "Playlist 1" mockUserId,
          Playlist "Playlist 2" mockUserId,
          Playlist "Playlist 3" mockUserId
        ]
  let dummyTracks = [ 
          -- Music "./assets/virtual_insanity.mp3" "Virtual Insanity" "Jamiroquai" currentTime "Travelling Without Moving" 5000 329,
          -- Music "./assets/paint_it_black.mp3" "Paint It Black" "The Rolling Stones" currentTime "Aftermath" 4000 202,
          -- Music "./assets/dont_stop_me_now.mp3" "Don't Stop Me Now" "Queen" currentTime "Jazz" 6000 210
          -- Music "./assets/stairway_to_heaven.mp3" "Stairway to Heaven" "Led Zeppelin" currentTime "Led Zeppelin IV" 5500 482 currentTime,
          -- Music "./assets/bohemian_rhapsody.mp3" "Bohemian Rhapsody" "Queen" currentTime "A Night at the Opera" 6200 354 currentTime
        ]
  return (dummyPlaylists, dummyTracks)

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  (playlists, tracks) <- runDb createDummyData
  liftIO $ restApi playlists tracks

  -- idUser <- insertUser "teste@gmail.com" "teste" "t" "senha"
  -- idUser2 <- insertUser "testeteste@gmail.com" "testeteste" "tt" "senhaa"
  -- updateUserName idUser "teste" "tes"
  -- deleteUser idUser

  -- idPlaylist <- insertPlaylist "Musicas para dormir" idUser2
  -- updatePlaylistName idPlaylist "Musicas para dancar"
  -- deletePlaylist idPlaylist

  -- idPlaylist1 <- insertPlaylist "Musicas legais" idUser2
  -- idPlaylist2 <- insertPlaylist "Musicas tristes" idUser2
  -- idPlaylist3 <- insertPlaylist "Musicas para quebrar tudo" idUser2
  -- idPlaylist4 <- insertPlaylist "Musicas divertidas" idUser2

  -- playlists <- selectAllPlaylists
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist) playlists

  -- playlistsAna <- selectPlaylistByAuthor "Ana"
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist ++ " Autor: " ++ playlistAuthor playlist) playlistsAna

  -- updateMusicName idMusic "novo nome"
  -- updateMusicAuthor idMusic "novo autor"
  -- updateMusicAlbum idMusic "novo album"
  -- updateMusicFilePath idMusic "novo path"
  -- let newReleaseDate = UTCTime (fromGregorian 2018 06 01) 0
  -- updateMusicReleaseDate idMusic newReleaseDate
  -- updateMusicFileSize idMusic 200
  -- updateMusicLength idMusic 300

  -- deleteMusic idMusic

  -- idMusic1 <- insertMusic "FilePath" "Believer" "autor1" releaseDate "album" 20 3
  -- idMusic2 <- insertMusic "FilePath" "Run" "autor2" releaseDate "album" 20 3
  -- idMusic3 <- insertMusic "FilePath" "Radioactive" "autor3" releaseDate "albumLegal" 20 3

  -- songs <- selectAllSongs
  -- liftIO $ mapM_ (\(Entity _ song) -> putStrLn $ "Nome: " ++ musicName song) songs

  -- musicasLegais <- selectMusicByName "Run"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasLegais

  -- musicasAutor <- selectMusicByAuthor "autor2"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAutor

  -- musicasAlbum <- selectMusicByAlbum "album"
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasAlbum

  -- let queryDate = UTCTime (fromGregorian 2018 06 21) 0
  -- musicasData <- selectMusicByRelesate queryDate
  -- liftIO $ mapM_ (\(Entity _ music) -> putStrLn $ "Nome: " ++ musicName music ++ " Autor: " ++ musicAuthor music) musicasData

  -- idRelation <- insertRelation idPlaylist1 idMusic2
  -- idRelation2 <- insertRelation idPlaylist1 idMusic2
  -- idRelation3 <- insertRelation idPlaylist1 idMusic2
  -- deleteRelation idRelation3

  -- idRelation4 <- insertRelation idPlaylist1 idMusic1
  -- idRelation5 <- insertRelation idPlaylist1 idMusic3
  -- idRelation6 <- insertRelation idPlaylist2 idMusic3
  -- idRelation7 <- insertRelation idPlaylist2 idMusic1

  -- musicasPlaylist1 <- selectRelationByPlaylist idPlaylist1
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Musica: " ++ show (relationMusic relation)) musicasPlaylist1

  -- musicasPlaylist2 <- selectRelationByPlaylist idPlaylist2
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Musica: " ++ show (relationMusic relation)) musicasPlaylist2

  -- inWhatPlaylistMusic1 <- selectRelationByMusic idMusic3
  -- liftIO $ mapM_ (\(Entity _ relation) -> putStrLn $ "Playlist: " ++ show (relationPlaylist relation)) inWhatPlaylistMusic1

  return ()
