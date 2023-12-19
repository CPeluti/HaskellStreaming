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
    thumbnail ByteString
    length Int
    created UTCTime default=CURRENT_TIME
    deriving Show
  Playlist
    name String
    author String
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

main :: IO ()
main = runSqlite "teste.db" $ do
  runMigration migrateAll
  time <- (liftIO getCurrentTime)
  teste <- insert $ Playlist "Teste" "Eu" time
  return ()