{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RestApi (restApi) where

import            Data.Foldable (for_)

import            IHP.HSX.QQ
import            IHP.HSX.ConvertibleStrings ()
import            IHP.HSX.ToHtml (ToHtml)

import            Web.Scotty as Scotty
import            Text.Blaze.Html.Renderer.Text (renderHtml)
import            Text.Blaze.Html5 as H

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
import            Network.Wai.Middleware.Static (static)

import            Views.Pages.LoginPage (loginPage)
import            Views.Pages.MusicPlayerPage (musicPlayerPage)
import            Views.Pages.FileUploadPage (fileUploadPage)

import qualified  Database.Persist (Entity)

import           DatabaseHaspotifaskell
import Database.Persist (Entity(..))


-- hxPost :: AttributeValue -> Attribute
-- hxPost = customAttribute "hx-post"
-- hxSwap :: AttributeValue -> Attribute
-- hxSwap = customAttribute "hx-swap"

-- myButton :: Html
-- myButton = button ! hxPost "/clicked" ! hxSwap "outerHTML" $ "Click me"

baseHtml :: IHP.HSX.ToHtml.ToHtml a => a -> Html
baseHtml bodyContent= [hsx|
  <html>
    <head>
      <script src="https://unpkg.com/htmx.org@1.9.9" integrity="sha384-QFjmbokDn2DjBjq+fM+8LUIVrAgqcNW2s0PjAxHETgRn9l4fvX31ZxDxvwQnyMOX" crossorigin="anonymous"></script>
      <script src="https://cdn.tailwindcss.com"></script>
      <style type="text/tailwindcss">
        @layer utilities {
          .content-auto {
            content-visibility: auto;
          }
        }
      </style>
    </head>
    <body>
      {bodyContent}
    </body>
  </html>
|]

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
    middleware $ static
    get "/" $
      Scotty.html $ renderHtml $ baseHtml loginPage
    post "/clicked" $
      Scotty.html $ renderHtml $
        H.div $
          for_ (Prelude.map show dbData) $ \id ->
            componentButton $ toHtml id
    post "/login" $ do
        username <- Scotty.param "username"
        password <- Scotty.param "password"
        liftIO $ putStrLn $ "Username: " ++ username ++ ", Password: " ++ password
         -- TODO: implement authentication
        Scotty.redirect "/musicPage"
    get "/uploadPage" $ do
        Scotty.html $ renderHtml $ baseHtml fileUploadPage


    get "/music" $ do
      startRange <- parseStart <$> Scotty.header "range"
      endRange <- parseEnd <$> Scotty.header "range"
      absolutePath <- liftIO $ getAbsolutePath fPathRelative
      totalSize <- liftIO $ fileSize absolutePath

      Scotty.status status206
      Scotty.setHeader "Content-Type" "audio/mpeg"
      Scotty.setHeader "Content-Length" (T.pack $ show totalSize)
      Scotty.setHeader "Content-Range" (T.pack $ generateRange (checkStart (parseInt $ T.unpack startRange)) (checkEnd (parseInt (T.unpack endRange)) totalSize))
      Scotty.stream $ streamingBD $ generateStream absolutePath
    get "/musicPage" $ do
      -- users <- liftIO $ runDb $ selectAllUsers
      liftIO $ mapM_ (\(Entity _ user) -> putStrLn $ "Nome: " ++ userFirstName user) users
      Scotty.html $ renderHtml $ baseHtml musicPlayerPage
