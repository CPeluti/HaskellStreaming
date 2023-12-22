{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RestApi (restApi) where

import Data.Foldable (for_)
import qualified Data.Text.Lazy as T
-- import Database.Persist (Entity (..))
-- import qualified Database.Persist (Entity)
import DatabaseHaspotifaskell
import IHP.HSX.ConvertibleStrings ()
import Network.HTTP.Types (status206)
import Network.Wai.Middleware.Static (static)
import Streaming
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Views.Pages.FileUploadPage (fileUploadPage)
import Views.Pages.LoginPage (loginPage)
import Views.Pages.MusicPlayer.MusicPlayerPage (musicPlayerPage, trackListTable)
import Views.Pages.MusicPlayer.CurrentlyPlayingBar (currentlyPlayingBar)
import Web.Scotty as Scotty
import Data.Time (getCurrentTime)
import Utils (baseHtml, dbData, componentButton, parseStart, parseEnd, getAbsolutePath, fPathRelative, fileSize, generateRange, checkStart, parseInt, checkEnd, streamingBD, generateStream, filterTracks)

restApi :: [Playlist] -> [Music] -> IO ()
restApi playlists tracks = do
  scotty 3000 $ do
    middleware static
    Scotty.get "/" $
      Scotty.html $
        renderHtml $
          baseHtml loginPage
    post "/clicked" $
      Scotty.html $
        renderHtml $
          H.div $
            for_ (Prelude.map show dbData) $ \id ->
              componentButton $ toHtml id
    post "/login" $ do
      username <- Scotty.param "username"
      password <- Scotty.param "password"
      liftIO $ putStrLn $ "Username: " ++ username ++ ", Password: " ++ password
      -- TODO: implement authentication
      Scotty.redirect "/musicPage"
    Scotty.get "/uploadPage" $ do
      Scotty.html $ renderHtml $ baseHtml fileUploadPage

    Scotty.get "/music" $ do
      startRange <- parseStart <$> Scotty.header "range"
      endRange <- parseEnd <$> Scotty.header "range"
      absolutePath <- liftIO $ getAbsolutePath fPathRelative
      totalSize <- liftIO $ fileSize absolutePath

      Scotty.status status206
      Scotty.setHeader "Content-Type" "audio/mpeg"
      Scotty.setHeader "Content-Length" (T.pack $ show totalSize)
      Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack startRange)) (checkEnd (parseInt (T.unpack endRange)) totalSize))

      Scotty.stream $ streamingBD $ generateStream absolutePath
    get "/musicPage" $ do
      -- users <- liftIO $ runDb $ selectAllUsers
      -- liftIO $ mapM_ (\(Entity _ user) -> putStrLn $ "Nome: " ++ userFirstName user) users
      Scotty.html $ renderHtml $ baseHtml $ musicPlayerPage playlists tracks
    Scotty.post "/changeTrack" $ do
      filePath <- Scotty.param "filePath"
      name <- Scotty.param "name"
      author <- Scotty.param "author"

      -- Use current time as a placeholder for releaseDate
      currentTime <- liftIO getCurrentTime

      let album = "Unknown Album"
      let fileSize = 0  -- Placeholder value for file size
      let fileLength = 0  -- Placeholder value for file length

      let currentTrack = Music filePath name author currentTime album fileSize fileLength currentTime

      Scotty.html $ renderHtml $ currentlyPlayingBar currentTrack
    Scotty.get "/search-tracks" $ do
      query <- Scotty.param "searchQuery" `Scotty.rescue` \_ -> return ""
      let filteredTracks = filterTracks query tracks

      -- Print the length of the filtered tracks array to the console
      liftIO $ putStrLn $ "Number of filtered tracks: " ++ show (Prelude.length filteredTracks)

      Scotty.html $ renderHtml $ trackListTable filteredTracks
