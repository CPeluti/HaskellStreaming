{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Common (inputField, button, playButton) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html

-- Input field component
inputField :: String -> String -> String -> Html.Html
inputField fieldType name placeholder = [hsx|
    <div>
        <input class="bg-gray-700 w-full p-4 rounded text-white focus:outline-none focus:shadow-outline" 
               type={fieldType} id={name} name={name} placeholder={placeholder} required>
    </div>
|]

-- Generic button component
button :: String -> String -> Html.Html
button fieldType label = [hsx|
    <button class="w-full p-4 bg-purple-500 text-white rounded focus:outline-none focus:shadow-outline hover:bg-green-600 transition-colors" 
            type={fieldType}>
        {label}
    </button>
|]

-- Play button component
playButton :: Html.Html
playButton = [hsx|
    <button class="bg-transparent border border-white text-white p-2 rounded-full focus:outline-none focus:border-gray-500 hover:bg-white hover:text-gray-900 transition-colors duration-150 ease-in-out">
        <svg viewBox="0 0 24 24" class="h-6 w-6 fill-current">
            <path d="M8 5v14l11-7z" /> <!-- This is a simple play icon -->
        </svg>
    </button>
|]
