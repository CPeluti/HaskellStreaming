{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Layouts (mainLayout) where

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
