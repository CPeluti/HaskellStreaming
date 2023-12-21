
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.MusicPlayer.MusicPlayerPage (musicPlayerPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Layouts as Layouts
import Views.Pages.MusicPlayer.CurrentlyPlayingBar (currentlyPlayingBar)
import DatabaseHaspotifaskell (User(..), Music(..), Playlist(..), insertUser)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)

-- import qualified Views.Common as Common

musicPlayerPage :: [Playlist] -> [Music] -> Html.Html
musicPlayerPage dummyPlaylists dummyTracks =
  Layouts.mainLayout
    [hsx|
        <script>
        function togglePlayPause(audioId) {
            var audio = document.getElementById(audioId);
            var playIcon = document.getElementById('play-icon');
            var pauseIcon = document.getElementById('pause-icon');

            if (audio.paused) {
                audio.play();
                playIcon.style.display = 'none';
                pauseIcon.style.display = 'block';
            } else {
                audio.pause();
                playIcon.style.display = 'block';
                pauseIcon.style.display = 'none';
            }
        }
        </script>
        <div class="flex flex-col lg:flex-row h-screen bg-gray-900 text-white">
        {sidebar dummyPlaylists}
        <div class="flex flex-col flex-grow">
            {mainContent dummyTracks}
            {currentlyPlayingBar}
        </div>
    </div>
|]


mainContent :: [Music] -> Html.Html
mainContent tracks =
  [hsx|
    <div class="flex-grow overflow-y-auto">
        {albumCoverSection}
        {trackListTable tracks}
    </div>
  |]

albumCoverSection :: Html.Html
albumCoverSection = 
  [hsx|
    <div class="p-8 flex flex-col lg:flex-row items-center bg-gradient-to-r from-indigo-500">
        <!-- Album Cover -->
        <img src="/assets/imagine.jpeg" alt="Song cover" class="w-48 h-48 mr-8 mb-4 lg:mb-0 shadow-lg rounded-lg"/>
        <!-- Playlist Info -->
        <div class="text-center lg:text-left">
            <h2 class="text-2xl font-bold text-gray-800 dark:text-white">Playlist 1</h2>
            <p class="text-sm text-gray-600 dark:text-gray-400">A selection of your favorite tracks</p>
            <div class="mt-4">
                <!-- {playButton "player"} -->
            </div>
        </div>
    </div>
  |]

trackListTable :: [Music] -> Html.Html
trackListTable tracks =
  [hsx|
    <div class="min-h-[30%] overflow-x-auto bg-gradient-to-b to-indigo-900 from-10% to-90%">
        <table class="w-full text-left border-collapse">
            <thead class="text-xs font-semibold uppercase bg-transparent">
                <tr>
                    <th class="p-2">Track</th>
                    <th class="p-2">Album</th>
                    <th class="p-2">Author</th>
                    <th class="p-2">Release Date</th>
                    <th class="p-2">File Size</th>
                    <th class="p-2">Length</th>
                </tr>
            </thead>
            <tbody class="text-sm divide-y divide-gray-200 dark:divide-gray-700">
                {mapM_ renderTrack tracks}
            </tbody>
        </table>
    </div>
  |]

sidebar :: [Playlist] -> Html.Html
sidebar playlists =
  [hsx|
    <div class="bg-black lg:w-64 w-full h-20 lg:h-screen overflow-y-auto lg:overflow-hidden">
        <div class="text-white p-5 hidden lg:block">Minha Biblioteca</div>
        <ul class="flex lg:flex-col overflow-x-auto lg:overflow-x-hidden">
            {mapM_ renderPlaylist playlists}
        </ul>
    </div>
|]



renderPlaylist :: Playlist -> Html.Html
renderPlaylist (Playlist name authorId created) =
  [hsx|
    <li class="p-2 hover:bg-gray-800 lg:block">
        <div>{name}</div>
        <div class="text-xs text-gray-400">Created by {authorId} on {formatTime defaultTimeLocale "%Y-%m-%d" created}</div>
       <!-- <div class="text-xs text-gray-400">Created by {userFirstName author} {userLastName author} on {formatTime defaultTimeLocale "%Y-%m-%d" created}</div> -->
    </li>
  |]

renderTrack :: Music -> Html.Html
renderTrack (Music filePath name author releaseDate album fileSize length _) =
  [hsx|
    <tr class="hover:bg-gray-800">
        <td class="p-2">{name}</td>
        <td class="p-2">{album}</td>
        <td class="p-2">{author}</td>
        <td class="p-2">{formatTime defaultTimeLocale "%Y-%m-%d" releaseDate}</td>
        <td class="p-2">{show fileSize} KB</td>
        <td class="p-2">{formatDuration length}</td>
    </tr>
  |]

formatDuration :: Int -> String
formatDuration seconds = let minutes = seconds `div` 60
                             remainingSeconds = seconds `mod` 60
                         in printf "%02d:%02d" minutes remainingSeconds
