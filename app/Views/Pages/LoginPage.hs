{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.LoginPage (loginPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Common as Common
import qualified Views.Layouts as Layouts

-- Login page
loginPage :: Html.Html
loginPage = Layouts.mainLayout [hsx|
    <div class="bg-gray-800 p-10 rounded-lg shadow-lg w-72">
        <h1 class="text-4xl mb-5 text-center">Login</h1>
        {loginForm}
    </div>
|]
 
loginButton :: Html.Html
loginButton = Common.button "Login" "submit"

-- Login form used in the login page
loginForm :: Html.Html
loginForm = [hsx|
    <form class="space-y-5" method="post" action="/login">
        {Common.inputField "text" "username" "Username"}
        {Common.inputField "password" "password" "Password"}
        {loginButton}
    </form>
|]
