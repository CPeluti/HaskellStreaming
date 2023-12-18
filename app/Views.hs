{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views (loginPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html

loginPage :: Html.Html
loginPage = [hsx|
    <html>
        <body class="bg-gray-900 text-white font-sans flex justify-center items-center h-screen m-0">
            <div class="bg-gray-800 p-10 rounded-lg shadow-lg w-72">
                <h1 class="text-4xl mb-5 text-center">Login</h1>
                {loginForm}
            </div>
        </body>
    </html>
|]

loginForm :: Html.Html
loginForm = [hsx|
    <form class="space-y-5" method="post" action="/login">
        {inputField "text" "username" "Username"}
        {inputField "password" "password" "Password"}
        {loginButton}
    </form>
|]

inputField :: String -> String -> String -> Html.Html
inputField fieldType name placeholder = [hsx|
    <div>
        <input class="bg-gray-700 w-full p-4 rounded text-white focus:outline-none focus:shadow-outline" 
               type={fieldType} id={name} name={name} placeholder={placeholder} required>
    </div>
|]

loginButton :: Html.Html
loginButton = [hsx|
    <div>
        <button class="w-full p-4 bg-purple-500 text-white rounded focus:outline-none focus:shadow-outline hover:bg-green-600 transition-colors" 
                type="submit">
            Login
        </button>
    </div>
|]
