{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RestApi (restApi) where

import            Data.Foldable (for_)

import            IHP.HSX.QQ
import            IHP.HSX.ConvertibleStrings ()
import            IHP.HSX.ToHtml (ToHtml)

import            Web.Scotty as Scotty
import            Text.Blaze.Html.Renderer.Text (renderHtml)
import            Text.Blaze.Html5 as H

import qualified  Data.Text.Lazy as T
import qualified  Data.Text.Encoding

import            Network.HTTP.Types (status206, status404)
import            Text.Read (readMaybe)
import            Control.Monad.Trans.Resource
import            Network.Wai
import            Streaming
import qualified  Streaming.Prelude               as S
import            Data.ByteString.Builder (byteString)
import            Data.ByteString as B
import qualified  Data.ByteString.Char8 as BSL
import            Streaming.ByteString  as BSS (toChunks, readFile)
import            System.Directory (getCurrentDirectory)
import            System.IO (IOMode(..), hFileSize, withFile)
import            Network.Wai.Middleware.Static (static)

import            Views.Pages.LoginPage (loginPage)
import            Views.Pages.MusicPlayer.MusicPlayerPage (musicPlayerPage, trackListTable)
import            Views.Pages.FileUploadPage (fileUploadPage)

import Network.Wai.Parse

import qualified  Data.ByteString.Lazy as BLazy
import           DatabaseHaspotifaskell
import           Database.Persist (Entity(..))
import qualified Database.Persist.Sqlite as DB
import           Models
import           ModelsJson
import           Data.Int
import           Data.Typeable

import Data.Time



import Views.Pages.MusicPlayer.CurrentlyPlayingBar (currentlyPlayingBar)
import Utils (understandTime, baseHtml, dbData, componentButton, parseStart, parseEnd, getAbsolutePath, fileSize, generateRange, checkStart, parseInt, checkEnd, streamingBD, generateStream, filterTracks)





import Control.Exception.Lifted

-- streamMusic :: Music -> IO()
-- streamMusic m = do
--   filePath <- musicFilePath m
--   startRange <- parseStart <$> Scotty.header "range"
--   endRange <- parseEnd <$> Scotty.header "range"
--   absolutePath <- liftIO $ getAbsolutePath filePath
--   totalSize <- liftIO $ fileSize absolutePath

--   Scotty.status status206
--   Scotty.setHeader "Content-Type" "audio/mpeg"
--   Scotty.setHeader "Content-Length" (T.pack $ show totalSize)
--   Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack startRange)) (checkEnd (parseInt (T.unpack endRange)) totalSize))
--   Scotty.stream $ streamingBD $ generateStream absolutePath

musicFolder = "musics/"
restApi :: IO ()
restApi  = do
  scotty 3000 $ do
    middleware static
    get "/" $ do
      -- users <- liftIO $ runDb $ selectAllUsers
      -- liftIO $ mapM_ (\(Entity _ user) -> putStrLn $ "Nome: " ++ userFirstName user) users
      tracks <- liftIO $ runDb selectAllSongs
      playlists <- liftIO $ runDb selectAllPlaylists
      Scotty.html $ renderHtml $ baseHtml $ musicPlayerPage playlists tracks
    post "/clicked" $
      Scotty.html $
        renderHtml $
          H.div $
            for_ (Prelude.map show dbData) $ \id ->
              componentButton $ toHtml id

    Scotty.get "/uploadPage" $ do
      Scotty.html $ renderHtml $ baseHtml fileUploadPage

    get "/music/:id" $ do
      (idValue :: Int64) <- Scotty.param "id"
      (music :: Maybe Music) <- liftIO $ runDb $ DB.get $ DB.toSqlKey $ idValue
      case music of
        Just (m) -> do
          -- retira aspas colocadas pelo banco de dados
          filePath <- evaluate $ Prelude.filter (/='"') $ musicFilePath m
          startRange <- parseStart <$> Scotty.header "range"
          endRange <- parseEnd <$> Scotty.header "range"
          absolutePath <- liftIO $ getAbsolutePath filePath
          totalSize <- liftIO $ fileSize absolutePath
          Scotty.status status206
          Scotty.setHeader "Content-Type" "audio/mpeg"
          Scotty.setHeader "Content-Length" (T.pack $ show totalSize)
          Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack startRange)) (checkEnd (parseInt (T.unpack endRange)) totalSize))
          Scotty.stream $ streamingBD $ generateStream absolutePath
        Nothing -> Scotty.status status404
      -- liftIO $ print $ MusicName $ entityVal music
    
    post "/music" $ do
      fs <- files

      (song :: String) <- Scotty.param "song_name"
      (author :: String)  <- Scotty.param "author_name"
      (album :: String) <- Scotty.param "album_name"
      (releaseDate :: String) <- Scotty.param "release_date"
      (length :: Int) <- Scotty.param "length"
      -- use bd key as name on drive
      lastSong <- liftIO $ runDb $ (DB.selectList [] [DB.Desc MusicId, DB.LimitTo 1]) 
      name <- case lastSong of
        [] -> evaluate 1
        [x] -> evaluate ((DB.fromSqlKey . DB.entityKey $ x :: Int64) + 1)
        (x:xs) -> evaluate ((DB.fromSqlKey . DB.entityKey $ x :: Int64) + 1)
      filePath <- evaluate $ musicFolder ++ (show name)
      --
      -- write on drive
      let fs1 = [ (musicName, fileContent file) | (musicName, file)<- fs]
      _ <- liftIO $ sequence_ [BLazy.writeFile filePath fileContent | (_, fileContent) <- fs1]
      --
      -- get filesize
      fSize <- liftIO $ fromIntegral <$> withFile filePath ReadMode hFileSize
      -- write on db
      _ <- liftIO $ runDb $ (insertMusic filePath song author (understandTime releaseDate :: UTCTime) album fSize length)
      --
      Scotty.text "foi"
      -- (music :: Music) <- jsonData
      -- uid <- liftIO $ runDb $ DB.insert music
      -- json $ Entity uid music

    Scotty.post "/changeTrack" $ do
      filePath <- Scotty.param "filePath"
      name <- Scotty.param "name"
      author <- Scotty.param "author"
      currentTime <- liftIO getCurrentTime

      let album = "Unknown Album"
      let fileSize = 0  -- Placeholder value for file size
      let fileLength = 0  -- Placeholder value for file length

      let currentTrack = Music filePath name author currentTime album fileSize fileLength

      Scotty.html $ renderHtml $ currentlyPlayingBar currentTrack

    Scotty.get "/search-tracks" $ do
    
      query <- Scotty.param "searchQuery" `Scotty.rescue` \_ -> return ""

      tracks <- liftIO $ runDb selectAllSongs
      let filteredTracks = filterTracks query tracks

      Scotty.html $ renderHtml $ trackListTable filteredTracks
