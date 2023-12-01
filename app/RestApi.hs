{-# LANGUAGE OverloadedStrings #-}
module RestApi where

import Data.Foldable (for_)

import Web.Scotty as Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

hxPost content = customAttribute "hx-post" content
hxSwap content = customAttribute "hx-swap" content

myButton = button ! hxPost "/clicked" ! hxSwap "outerHTML" $ "Click me"

componentButton id = button $ id

dbData = [1 .. 4]

restApi :: IO ()
restApi = 
  scotty 3000 $ do
    get "/" $
      Scotty.html $ renderHtml $
        H.html $ do
          H.head $
            H.script ! src "https://unpkg.com/htmx.org@1.9.6" $ H.span ""
          H.body $
            myButton
    post "/clicked" $
      Scotty.html $ renderHtml $
        H.div $
          for_ (Prelude.map show dbData) $ \(id) ->
            componentButton $ toHtml id
