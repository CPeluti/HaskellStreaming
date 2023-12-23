{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.MusicPlayer.MusicPlayerPage (musicPlayerPage, trackListTable) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Layouts as Layouts
import Views.Pages.MusicPlayer.CurrentlyPlayingBar (currentlyPlayingBar)
import DatabaseHaspotifaskell (Music(..), Playlist(..))
import Database.Persist (Entity(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)
import Data.Maybe (listToMaybe)

musicPlayerPage :: [Entity Playlist] -> [Entity Music] -> Html.Html
musicPlayerPage dummyPlaylists entityTracks =
  Layouts.mainLayout
    [hsx| 
        <script src="https://unpkg.com/htmx.org@1.9.10"></script>
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
              {mainContent entityTracks}
              {maybeCurrentlyPlayingBar (listToMaybe entityTracks)}
          </div>
        </div>
|]

maybeCurrentlyPlayingBar :: Maybe (Entity Music) -> Html.Html
maybeCurrentlyPlayingBar (Just (Entity _ track)) = currentlyPlayingBar track
maybeCurrentlyPlayingBar Nothing = 
  [hsx|
    <div class="flex items-center justify-center h-full">
        <div class="text-center p-10">
            <div class="mb-4">
                <img src="https://www.neonvibes.co.uk/cdn/shop/products/Nothing-to-see-hereneonvibes.co.ukLEDneonsignsMadeintheUK_2000x.jpg?v=1677846952" alt="No Music Cat" class="mx-auto" style="width: 200px; height: auto;"/>
            </div>
            <h2 class="text-xl font-semibold text-gray-300">Oops, no tunes here!</h2>
            <p class="text-gray-400">Looks like the music took a break. Why not start something groovy?</p>
        </div>
    </div>
  |]

mainContent :: [Entity Music] -> Html.Html
mainContent entityTracks =
  [hsx|
    <div class="flex-grow overflow-y-auto">
        {albumCoverSection}
        {searchBarComponent}
        {trackListTable entityTracks}
    </div>
  |]

searchBarComponent :: Html.Html
searchBarComponent = 
  [hsx|
    <div class="py-2 px-2">
        <input type="text" name="searchQuery" placeholder="Search Tracks..." hx-get="/search-tracks" hx-params="searchQuery" hx-trigger="keyup changed delay:500ms" hx-target="#track-list-table" class="w-full pl-6 pr-4 py-2 rounded-full bg-black text-white focus:outline-none focus:ring-2 focus:ring-blue-500"/>
    </div>
  |]

albumCoverSection :: Html.Html
albumCoverSection =
  [hsx|
    <div class="p-8 flex flex-col lg:flex-row items-center bg-gradient-to-r from-indigo-500">
        <img src="/assets/imagine.jpeg" alt="Song cover" class="w-48 h-48 mr-8 mb-4 lg:mb-0 shadow-lg rounded-lg"/>
        <div class="text-center lg:text-left">
            <h2 class="text-2xl font-bold text-gray-800 dark:text-white">Playlist 1</h2>
            <p class="text-sm text-gray-600 dark:text-gray-400">A selection of your favorite tracks</p>
            <div class="mt-4">
                <!-- {playButton "player"} -->
            </div>
        </div>
    </div>
  |]

trackListTable :: [Entity Music] -> Html.Html
trackListTable entityTracks =
  [hsx|
    <div class="min-h-[30%] overflow-x-auto bg-gradient-to-b to-indigo-900 from-10% to-90%">
        <table id="track-list-table" class="w-full text-left border-collapse">
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
                {mapM_ renderTrack entityTracks}
            </tbody>
        </table>
    </div>
  |]

sidebar :: [Entity Playlist] -> Html.Html
sidebar entityPlaylists =
  [hsx|
    <div class="bg-black lg:w-64 w-full h-20 lg:h-screen overflow-y-auto lg:overflow-hidden">
        <div class="text-white p-5 hidden lg:block">Minha Biblioteca</div>
        <ul class="flex lg:flex-col overflow-x-auto lg:overflow-x-hidden">
            {mapM_ renderPlaylist entityPlaylists}
        </ul>
    </div>
  |]

-- Now takes an Entity Playlist and extracts the Playlist from it
renderPlaylist :: Entity Playlist -> Html.Html
renderPlaylist (Entity _ (Playlist name authorId)) =


renderPlaylist :: Playlist -> Html.Html
renderPlaylist (Playlist name) =
  [hsx|
    <li class="p-2 hover:bg-gray-800 lg:block">
        <div>{name}</div>
        <div class="text-xs text-gray-400">Created by {authorId}</div>
    </li>
  |]

renderTrack :: Entity Music -> Html.Html
renderTrack (Entity _ (Music filePath name author releaseDate album fileSize fileLength)) =
  let hxVals = "{\"filePath\": \"" ++ filePath ++ "\", \"name\": \"" ++ name ++ "\", \"author\": \"" ++ author ++ "\"}"
  in [hsx|
    <tr class="hover:bg-gray-800">
        <td class="p-2">{name}</td>
        <td class="p-2">{album}</td>
        <td class="p-2">{author}</td>
        <td class="p-2">{formatTime defaultTimeLocale "%Y-%m-%d" releaseDate}</td>
        <td class="p-2">{show fileSize} KB</td>
        <td class="p-2">{formatDuration fileLength}</td>
        <td class="p-2">
            <button hx-post="/changeTrack" hx-vals={hxVals} hx-target="#currently-playing-bar">
                Play
            </button>
        </td>
    </tr>
  |]

formatDuration :: Int -> String
formatDuration seconds = let minutes = seconds `div` 60
                             remainingSeconds = seconds `mod` 60
                         in printf "%02d:%02d" minutes remainingSeconds
