{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RestApi (restApi) where

import           Data.Foldable (for_)
import           Web.Scotty as Scotty
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import qualified Data.Text.Lazy as T

import Network.HTTP.Types (status206)
import Text.Read (readMaybe)
import           Control.Monad.Trans.Resource
import           Network.Wai
import           Streaming
import qualified Streaming.Prelude               as S
import           Data.ByteString.Builder (byteString)
import           Data.ByteString as B
import           Streaming.ByteString  as BSS (toChunks, readFile)


hxPost :: AttributeValue -> Attribute
hxPost = customAttribute "hx-post"
hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"

myButton :: Html
myButton = button ! hxPost "/clicked" ! hxSwap "outerHTML" $ "Click me"

componentButton :: Html -> Html
componentButton = button

dbData :: [Integer]
dbData = [1 .. 4]

fPath :: FilePath
fPath = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/app/eBG7P-K-r1Y_160.mp3"

fileSize :: FilePath -> IO Int
fileSize f = do
  readedFile <-  B.readFile f
  return $ B.length readedFile

-- unsafeFSize :: FilePath -> String
-- unsafeFSize f = show $ unsafePerformIO $ fileSize f

-- fileSize = 5827011

generateStream :: MonadResource m => FilePath -> Stream (Of ByteString) m ()
generateStream f = toChunks $ BSS.readFile f

--TODO: refatorar
parseRanges :: Maybe T.Text -> [T.Text]
parseRanges (Just f) = T.splitOn "-" $ Prelude.last $ T.splitOn "bytes=" f
parseRanges Nothing = ["",""]

parseStart :: Maybe T.Text -> T.Text
parseStart f = Prelude.head $ parseRanges f
parseEnd :: Maybe T.Text -> T.Text
parseEnd f = Prelude.head $ Prelude.tail $ parseRanges f

checkEnd :: Maybe Int -> Int -> Int
checkEnd Nothing maxLength = maxLength - 1
checkEnd (Just x) _ = x

checkStart :: Maybe Int -> Int
checkStart Nothing = 0
checkStart (Just x) = x

parseInt :: String -> Maybe Int
parseInt s = readMaybe s :: Maybe Int
--

generateRange :: Int -> Int -> String
generateRange partial_start partial_end = "bytes " ++ show partial_start ++ "-" ++ show partial_end ++ "/" ++ show (partial_end-partial_start+1)

streamingBD :: Stream (Of ByteString) (ResourceT IO) r -> StreamingBody
streamingBD s =
  streamingBody
  where
  streamingBody writeBuilder flush = runResourceT $ void $ S.effects $ S.for s writer
    where
    writer aux = do 
        _ <-liftIO (writeBuilder (byteString aux))
        liftIO flush

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
          for_ (Prelude.map show dbData) $ \id ->
            componentButton $ toHtml id
    get "/music" $ do
      start <- parseStart <$> Scotty.header "range"
      end <- parseEnd <$> Scotty.header "range"
      fSize <- liftIO $ fileSize fPath
      -- start <- 
      Scotty.status status206
      Scotty.setHeader "Content-Type" "audio/mpeg"
      Scotty.setHeader "Content-Length" (T.pack $ show fSize)
      Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack start)) (checkEnd (parseInt (T.unpack end)) fSize))
      Scotty.stream $ streamingBD $ generateStream fPath
