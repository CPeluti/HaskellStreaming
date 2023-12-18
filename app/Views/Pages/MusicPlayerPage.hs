{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.MusicPlayerPage (musicPlayerPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Layouts as Layouts
import qualified Views.Common as Common

musicPlayerPage :: Html.Html
musicPlayerPage = Layouts.mainLayout [hsx|
    <div class="flex flex-col lg:flex-row h-screen bg-gray-900 text-white">
        {sidebar dummyPlaylists}
        <div class="flex flex-col flex-grow">
            {mainContent dummyTracks}
            {currentlyPlayingBar}
        </div>
    </div>
|]

-- audioPlayer :: Html.Html
-- audioPlayer = [hsx|
--     <div class="p-4 ">
--         <audio controls>
--             <source src="/music" type="audio/mpeg"/>
--             Your browser does not support the audio element.
--         </audio>
--     </div>
-- |]

dummyPlaylists :: [String]
dummyPlaylists = ["Playlist 1", "Playlist 2", "Playlist 3", "Playlist 4", "Playlist 5"]

sidebar :: [String] -> Html.Html
sidebar playlists = [hsx|
    <div class="bg-black lg:w-64 w-full h-20 lg:h-screen overflow-y-auto lg:overflow-hidden">
        <div class="text-white p-5 hidden lg:block">Minha Biblioteca</div>
        <ul class="flex lg:flex-col overflow-x-auto lg:overflow-x-hidden">
            {mapM_ renderPlaylist playlists}
        </ul>
    </div>
|]

renderPlaylist :: String -> Html.Html
renderPlaylist playlistName = [hsx|
    <li class="p-2 hover:bg-gray-800 lg:block">{playlistName}</li>
|]

dummyTracks :: [(String, String)]
dummyTracks = [("Track 1", "Album 1"), ("Track 2", "Album 2"), ("Track 3", "Album 3")]

mainContent :: [(String, String)] -> Html.Html
mainContent tracks = [hsx|
    <div class="flex-grow overflow-y-auto">
        <div class="p-8 flex flex-col lg:flex-row items-center bg-gradient-to-r from-indigo-500">
            <!-- Album Cover -->
            <img src="/assets/imagine.jpeg" alt="Song cover" class="w-48 h-48 mr-8 mb-4 lg:mb-0 shadow-lg rounded-lg"/>
            <!-- Playlist Info -->
            <div class="text-center lg:text-left">
                <h2 class="text-2xl font-bold text-gray-800 dark:text-white">Playlist 1</h2>
                <p class="text-sm text-gray-600 dark:text-gray-400">A selection of your favorite tracks</p>
                <div class="mt-4">
                    {Common.playButton}
                </div>
            </div>
        </div>
        <!-- Track List Table -->
        <div class="min-h-[30%] overflow-x-auto bg-gradient-to-b to-indigo-900 from-10% to-90%">
            <table class="w-full text-left border-collapse">
                <thead class="text-xs font-semibold uppercase bg-transparent">
                    <tr>
                        <th class="p-2">Track</th>
                        <th class="p-2">Album</th>
                    </tr>
                </thead>
                <tbody class="text-sm divide-y divide-gray-200 dark:divide-gray-700">
                    {mapM_ renderTrack tracks}
                </tbody>
            </table>
        </div>
    </div>
|]

renderTrack :: (String, String) -> Html.Html
renderTrack (trackName, albumName) = [hsx|
    <tr class="hover:bg-gray-800">
        <td class="p-2">{trackName}</td>
        <td>{albumName}</td>
    </tr>
|]

currentlyPlayingBar :: Html.Html
currentlyPlayingBar = [hsx|
    <div class="bg-gray-800 p-4 flex lg:flex-row flex-col lg:fixed lg:inset-x-0 lg:bottom-0 w-full justify-between">
        <!-- Left Section: Song Information -->
        <div class="flex items-center mb-4 lg:mb-0">
            <img src="/assets/imagine.jpeg" alt="Song cover" class="w-16 h-16 mr-4"/>
            <div>
                <div class="text-lg">Nome da Musica</div>
                <div>Autor</div>
            </div>
        </div>
        
        <!-- Middle Section: Play Controls and Progress Bar -->
        <div class="flex-1 min-w-0">
            <!-- Sound Controls -->
            <div class="flex items-center justify-center">
                <button class="mx-4">⮜</button>
                {Common.playButton}
                <button class="mx-4">⮞</button>
                <!-- {audioPlayer} -->
            </div>
            <!-- Progress Bar Container -->
            <div class="flex flex-col items-center mb-4">
                <!-- Time Labels -->
                <div class="flex justify-between w-full max-w-[50%] mb-1">
                    <span class="text-base font-medium text-blue-700 dark:text-white">0:30</span>
                    <span class="text-sm font-medium text-blue-700 dark:text-white">1:30</span>
                </div>
                <!-- Progress Bar -->
                <div class="w-full max-w-[50%] bg-gray-200 rounded-full h-2.5 dark:bg-gray-700">
                    <div class="bg-blue-600 h-2.5 rounded-full" style="width: 33%"></div>
                </div>
            </div>
        </div>
        <!-- Right Section: Sound Volume Control -->
        <div class="flex items-center">
            <!-- Stylish Volume Slider -->
            <img class="mr-2" width="24" height="24" src="https://img.icons8.com/material-outlined/24/audio-skimming.png" alt="audio-skimming"/>

            <input type="range" min="0" max="100" value="50" class="w-24 h-1 bg-blue-700 rounded-lg appearance-none cursor-pointer dark:bg-blue-300">
            <img class="ml-1" width="50" height="50" src="https://img.icons8.com/ios-filled/50/audio-skimming.png" alt="audio-skimming"/>
        </div>
    </div>
|]



