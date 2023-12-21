{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Layouts (mainLayout, modalLayout) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html

mainLayout :: Html.Html -> Html.Html
mainLayout content = [hsx|
    <html>
        <body class="bg-gray-900 text-white font-sans m-0">
            {content}
        </body>
    </html>
|]

modalLayout :: String -> Html.Html -> Html.Html
modalLayout title content =  [hsx|
    <html>
        <body class="flex flex-col lg:flex-row h-screen bg-gray-900 text-white justify-center">
            <div class="flex flex-grow items-center justify-center p-8">
                <div class="max-w-lg w-full bg-gray-800 p-6 rounded-lg shadow">
                <h2 class="text-2xl font-bold mb-4 text-center text-white">{title}</h2>
                {content}
                </div>
            </div>
        </body>
    </html>
|]
