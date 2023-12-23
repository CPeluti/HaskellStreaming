{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utils (understandTime, baseHtml, dbData, componentButton, parseStart, parseEnd, getAbsolutePath, fileSize, generateRange, checkStart, parseInt, checkEnd, streamingBD, generateStream, filterTracks) where

import Control.Monad.Trans.Resource
import Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.Text.Lazy as T

-- import Database.Persist (Entity (..))
-- import qualified Database.Persist (Entity)
import DatabaseHaspotifaskell
import IHP.HSX.ConvertibleStrings ()
import IHP.HSX.QQ
import IHP.HSX.ToHtml (ToHtml)
import Network.Wai
import Streaming
import Streaming.ByteString as BSS (readFile, toChunks)
import qualified Streaming.Prelude as S
import System.Directory (getCurrentDirectory)
import Text.Blaze.Html5 as H
import Text.Read (readMaybe)
import Data.ByteString.Char8 as BC
-- import Text.FuzzyFind (bestMatch, Alignment(..))
import Text.FuzzyFind (bestMatch)
import Data.Time
import Database.Persist (Entity(..))

timeFormat :: String
timeFormat = "%Y-%-m-%-d"

understandTime:: String -> UTCTime
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

baseHtml :: (IHP.HSX.ToHtml.ToHtml a) => a -> Html
baseHtml bodyContent =
  [hsx|
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

-- Function to get the absolute path
getAbsolutePath :: FilePath -> IO FilePath
getAbsolutePath anyRelativePath = do
  currentDir <- getCurrentDirectory
  return (currentDir ++ "/" ++ anyRelativePath)

fileSize :: FilePath -> IO Int
fileSize f = do
  readedFile <- B.readFile f
  return $ B.length readedFile

generateStream :: (MonadResource m) => FilePath -> Stream (Of ByteString) m ()
generateStream f = toChunks $ BSS.readFile f

-- TODO: refatorar
parseRanges :: Maybe T.Text -> [T.Text]
parseRanges (Just f) = T.splitOn "-" $ Prelude.last $ T.splitOn "bytes=" f
parseRanges Nothing = ["", ""]

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

-- TODO: Refatorar
generateRange :: Int -> Int -> String
generateRange partial_start partial_end = "bytes " ++ show partial_start ++ "-" ++ show partial_end ++ "/" ++ show (partial_end - partial_start + 1)

streamingBD :: Stream (Of ByteString) (ResourceT IO) r -> StreamingBody
streamingBD s =
  streamingBody
  where
    streamingBody writeBuilder flush = runResourceT $ void $ S.effects $ S.for s writer
      where
        writer aux = do
          _ <- liftIO (writeBuilder (byteString aux))
          liftIO flush


filterTracks :: ByteString -> [Entity Music] -> [Entity Music]
filterTracks query entityTracks =
  let queryString = BC.unpack query
  in Prelude.filter (isFuzzyMatch queryString . musicName . entityVal) entityTracks

isFuzzyMatch :: String -> String -> Bool
isFuzzyMatch query trackName =
  case bestMatch query trackName of
    Just _alignment -> True  -- or use 'score' from 'Alignment' to set a threshold
    Nothing -> False
