{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}

module ModelsJson where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Database.Persist
import           Models

instance ToJSON (Entity Music) where
    toJSON (Entity uid (c@Music{..})) =
        object
        [ "id" .= uid
        , "filePath" .= musicFilePath 
        , "name" .= musicName 
        , "author" .= musicAuthor 
        , "releaseDate" .= musicReleaseDate 
        , "album" .= musicAlbum 
        , "fileSize" .= musicFileSize 
        , "length" .= musicLength 
        ]

instance FromJSON Music where
    parseJSON (Object v) =
        Music <$> v .: "filePath"
              <*> v .: "name"
              <*> v .: "author"
              <*> v .: "releaseDate"
              <*> v .: "album"
              <*> v .: "fileSize"
              <*> v .: "length"
    parseJSON _ = mzero