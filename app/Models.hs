
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Models where

import Control.Applicative
import Control.Monad

import Data.Aeson

import qualified Data.Text as T
import           Data.Time
import           Data.ByteString

import Database.Persist
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    email String
    firstName String 
    lastName String
    password String
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
    deriving Show
  Playlist
    name String
    author UserId
    deriving Show
  Relation
    playlist PlaylistId
    music MusicId
    deriving Show
|]