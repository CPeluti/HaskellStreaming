{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Common (inputField, loginButton) where

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

-- Login button component
loginButton :: Html.Html
loginButton = [hsx|
    <div>
        <button class="w-full p-4 bg-purple-500 text-white rounded focus:outline-none focus:shadow-outline hover:bg-green-600 transition-colors" 
                type="submit">
            Login
        </button>
    </div>
|]
