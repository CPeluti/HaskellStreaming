-- app/Views.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views (loginPage) where

import IHP.HSX.QQ (hsx)
import Prelude
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Login Page
loginPage :: Html
loginPage classes = [hsx|
    <html>
        <head>
            <title>Login</title>
            <link rel="stylesheet" type="text/css" href="/static/styles.css">
        </head>
        <body class="login-body">
            <div class="login-container">
                <h1 class="login-title">Login</h1>
                <form class="login-form" method="post" action="/login">
                    <div class="input-container">
                        <input type="text" id="username" name="username" placeholder="Username" required>
                    </div>
                    <div class="input-container">
                        <input type="password" id="password" name="password" placeholder="Password" required>
                    </div>
                    <div class="button-container">
                        <button class="bg-violet-400 hover:bg-green-500 border-none text-white font-bold" type="submit">Login</button>
                    </div>
                </form>
            </div>
        </body>
    </html>
|]
