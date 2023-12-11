module Lib
    ( someFunc
    ) where
import qualified Data.ByteString as B
someFunc :: IO ()
someFunc = putStrLn "someFunc"

teste f = do 
    res <- B.readFile f
    print res
