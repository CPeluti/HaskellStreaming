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
                <form class="space-y-5" method="post" action="/login">
                    <div>
                        <input class="bg-gray-700 w-full p-4 rounded text-white focus:outline-none focus:shadow-outline" 
                               type="text" id="username" name="username" placeholder="Username" required>
                    </div>
                    <div>
                        <input class="bg-gray-700 w-full p-4 rounded text-white mb-4 focus:outline-none focus:shadow-outline" 
                               type="password" id="password" name="password" placeholder="Password" required>
                    </div>
                    <div>
                        <button class="w-full p-4 bg-purple-500 text-white rounded focus:outline-none focus:shadow-outline hover:bg-green-600 transition-colors" 
                                type="submit">
                            Login
                        </button>
                    </div>
                </form>
            </div>
        </body>
    </html>
|]
