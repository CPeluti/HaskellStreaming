{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RestApi (restApi) where

import            Data.Foldable (for_)

import            IHP.HSX.QQ 
import            IHP.HSX.ConvertibleStrings () 
import            IHP.HSX.ToHtml () 

import            Web.Scotty as Scotty
import            Text.Blaze.Html.Renderer.Text (renderHtml)
import            Text.Blaze.Html5 as H
import            Text.Blaze.Html5.Attributes as A

import qualified  Data.Text.Lazy as T

import            Network.HTTP.Types (status206)
import            Text.Read (readMaybe)
import            Control.Monad.Trans.Resource
import            Network.Wai
import            Streaming
import qualified  Streaming.Prelude               as S
import            Data.ByteString.Builder (byteString)
import            Data.ByteString as B
import            Streaming.ByteString  as BSS (toChunks, readFile)
import            System.Directory (getCurrentDirectory)


hxPost :: AttributeValue -> Attribute
hxPost = customAttribute "hx-post"
hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"

myButton :: Html
myButton = button ! hxPost "/clicked" ! hxSwap "outerHTML" $ "Click me"

-- audioComponent = [hsx|
--   <audio controls>
--     <source src="horse.ogg" type="audio/ogg">
--     <source src="horse.mp3" type="audio/mpeg">
--     Your browser does not support the audio tag.
--   </audio>  
-- |]

componentButton :: Html -> Html
componentButton = button

dbData :: [Integer]
dbData = [1 .. 4]

fPathRelative :: FilePath
-- fPathRelative = "virtual_insanity_jamiroquai.mp3"
fPathRelative = "src/eBG7P-K-r1Y_160.mp3"

-- Function to get the absolute path
getAbsolutePath :: FilePath -> IO FilePath
getAbsolutePath anyRelativePath = do
    currentDir <- getCurrentDirectory
    return (currentDir ++ "/" ++ anyRelativePath)


fileSize :: FilePath -> IO Int
fileSize f = do
  readedFile <- B.readFile f
  return $ B.length readedFile

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

-- TODO: Refatorar
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
--

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
    -- get "/player" $
    --   Scotty.html $ renderHtml $
    --     H.div $
    --       audioComponent
          
          
    get "/music" $ do
      start <- parseStart <$> Scotty.header "range"
      end <- parseEnd <$> Scotty.header "range"
      absolutePath <- liftIO $ getAbsolutePath fPathRelative
      fSize <- liftIO $ fileSize absolutePath
      -- start <- 
      Scotty.status status206
      Scotty.setHeader "Content-Type" "audio/mpeg"
      Scotty.setHeader "Content-Length" (T.pack $ show fSize)
      Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack start)) (checkEnd (parseInt (T.unpack end)) fSize))
      Scotty.stream $ streamingBD $ generateStream absolutePath
