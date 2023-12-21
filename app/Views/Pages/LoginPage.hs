{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Pages.LoginPage (loginPage) where

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html as Html
import qualified Views.Common as Common
import qualified Views.Layouts as Layouts

loginPage :: Html.Html
loginPage = Layouts.modalLayout "Login" [hsx| {loginForm} |] 

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
