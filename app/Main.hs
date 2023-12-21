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
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.ByteString
import           Data.Time
import           Control.Monad.IO.Class
import           DatabaseHaspotifaskell


main :: IO ()
main = do
  runDb $ runMigration migrateAll
  restApi
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

  -- playlistsAna <- selectPlaylistByAuthor "Ana"
  -- liftIO $ mapM_ (\(Entity _ playlist) -> putStrLn $ "Nome: " ++ playlistName playlist ++ " Autor: " ++ playlistAuthor playlist) playlistsAna

  -- let releaseDate = UTCTime (fromGregorian 2018 06 21) 0
  -- idMusic <- insertMusic "FilePath" "musica" "autor" releaseDate "album" 20 3

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