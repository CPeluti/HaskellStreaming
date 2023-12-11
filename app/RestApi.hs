{-# LANGUAGE OverloadedStrings #-}
module RestApi where

import           Data.Foldable (for_)
import           Web.Scotty as Scotty
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Network.HTTP.Types
import           Control.Monad.Trans.Resource
import           Network.Wai
import           Streaming                   
import qualified Streaming.Prelude               as S
import           Data.ByteString.Builder (byteString, Builder)
import           Data.ByteString as B
import           Data.ByteString.Streaming  as BSS (toChunks, readFile)


hxPost content = customAttribute "hx-post" content
hxSwap content = customAttribute "hx-swap" content

myButton = button ! hxPost "/clicked" ! hxSwap "outerHTML" $ "Click me"

componentButton id = button $ id

dbData = [1 .. 4]

fileSize f = do
  file <- B.readFile f
  return $ B.length file

generateStream f = BSS.toChunks $ BSS.readFile f 

streamingBD :: Stream (Of ByteString) (ResourceT IO) r -> StreamingBody
streamingBD stream = 
  streamingBody 
  where
  streamingBody writeBuilder flush =
    let writer a = 
            do liftIO (writeBuilder (byteString a))
              -- flushes for every produced bytestring, perhaps not optimal
               liftIO flush
     in runResourceT $ void $ S.effects $ S.for stream writer

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
    get "/music" $ do
      Scotty.setHeader "Content-Type" "audio/mpeg"
      Scotty.setHeader "Content-Length" "5827011"
      Scotty.stream $ streamingBD $ generateStream "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/app/eBG7P-K-r1Y_160.mp3"
