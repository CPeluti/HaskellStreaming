{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.MusicPlayer.CurrentlyPlayingBar (currentlyPlayingBar) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import DatabaseHaspotifaskell

currentlyPlayingBar :: Music -> Html.Html
currentlyPlayingBar (Music filePath name author _ _ _ _length) =
  [hsx|
    <div id="currently-playing-bar">
      <div class="bg-gray-800 p-4 flex lg:flex-row flex-col lg:fixed lg:inset-x-0 lg:bottom-0 w-full justify-between">
          <!-- Left Section: Song Information -->
          {songInformation "/assets/imagine.jpeg" name author}

          <!-- Middle Section: Play Controls and Progress Bar -->
          <div class="flex-1 min-w-0">
              {audioPlayer filePath}
             <!-- {_soundControls} -->
             <!-- {_progressBar}   -->
          </div>

          <!-- Right Section: Sound Volume Control -->
        <!--{_volumeControls} -->
      </div>
    </div>
  |]

audioPlayer :: String -> Html.Html
audioPlayer filePath =
  [hsx|
    <div class="p-4 rounded-lg shadow flex flex-col items-center">
        <audio controls id="player" class="min-w-[75%]">
            <source src={filePath} type="audio/mpeg"/>
            Your browser does not support the audio element.
        </audio>
    </div>
  |]

playButton :: String -> Html.Html
playButton my_id =
  [hsx|
    <button onclick={"togglePlayPause('" ++ my_id ++ "')"} class="bg-transparent border border-white text-white p-2 rounded-full focus:outline-none focus:border-gray-500 hover:bg-white hover:text-gray-900 transition-colors duration-150 ease-in-out">
        <svg id="play-icon" viewBox="0 0 24 24" class="h-6 w-6 fill-current">
            <path d="M8 5v14l11-7z" /> <!-- This is a simple play icon -->
        </svg>
        <svg id="pause-icon" viewBox="0 0 24 24" class="h-6 w-6 fill-current" style="display: none;">
            <path d="M6 19h4V5H6v14zm8-14v14h4V5h-4z" /> <!-- Pause Icon -->
        </svg>
    </button>
|]

songInformation :: String -> String -> String -> Html.Html
songInformation coverImageUrl songName artistName =
  [hsx|
    <div class="flex items-center mb-4 lg:mb-0">
        <img src={coverImageUrl} alt="Song cover" class="w-16 h-16 mr-4"/>
        <div>
            <div class="text-lg">{songName}</div>
            <div>{artistName}</div>
        </div>
    </div>
  |]

-- Refactored progress bar
_progressBar :: Html.Html
_progressBar =
  [hsx|
    <div class="flex flex-col items-center mb-4">
        <div class="flex justify-between w-full max-w-[50%] mb-1">
            <span class="text-base font-medium text-blue-700 dark:text-white">0:30</span>
            <span class="text-sm font-medium text-blue-700 dark:text-white">1:30</span>
        </div>
        <div class="w-full max-w-[50%] bg-gray-200 rounded-full h-2.5 dark:bg-gray-700">
            <div class="bg-blue-600 h-2.5 rounded-full" style="width: 33%"></div>
        </div>
    </div>
  |]

-- Refactored volume controls
_volumeControls :: Html.Html
_volumeControls =
  [hsx|
    <div class="flex items-center">
        <img class="mr-2" width="24" height="24" src="https://img.icons8.com/material-outlined/24/audio-skimming.png" alt="audio-skimming"/>
        <input type="range" min="0" max="100" value="50" class="w-24 h-1 bg-blue-700 rounded-lg appearance-none cursor-pointer dark:bg-blue-300">
        <img class="ml-1" width="50" height="50" src="https://img.icons8.com/ios-filled/50/audio-skimming.png" alt="audio-skimming"/>
    </div>
  |]

-- Refactored sound controls
_soundControls :: Html.Html
_soundControls =
  [hsx|
    <div class="flex items-center justify-center">
        <button class="mx-4">⮜</button>
        {playButton "player"}
        <button class="mx-4">⮞</button>
    </div>
  |]