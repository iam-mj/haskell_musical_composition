{-# LANGUAGE OverloadedStrings #-}
module Visual.OpenHtml where
import Visual.UploadFile

import System.Process (callCommand)
import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import GHC.Conc (threadDelay)
import Data.List.Split (splitOn)
import Data.List (stripPrefix)
import Network.HTTP.Types (urlEncode)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Visual.CreateScore (launch)

serverPort :: Int
serverPort    = 3000
staticFolder  = "src/resources"
scorePath     = "index.html"
visPath       = "vis.html"
baseURL       = "http://localhost:" ++ show serverPort
sourceURL     = "?src="
nameURL       = "&name="
tempPrefix    = "temp_" 
tempSuffix    = ".mid" :: String

-- note: scotty port is blocking, so we start the server in a different thread
startServer :: IO ()
startServer = do
    void $ forkIO $ scotty serverPort $ do
        middleware $ staticPolicy (addBase staticFolder)
        get "/" $ file $ staticFolder ++ "/" ++ scorePath

-- note: strip a temp file of both its prefix and suffix
stripTemp :: String -> String
stripTemp name = 
    let strippedName = stripPrefix tempPrefix name
    in case strippedName of
        Nothing       -> name
        Just stripped -> take (length stripped - length tempSuffix) stripped

-- note: second bool True => open the score, False => open the js visualizer
openHtml :: FilePath -> Bool -> Bool -> IO ()
openHtml fileName running vis = do
    -- upload the file to a file host
  uploadResult <- uploadRequest fileName
  case uploadResult of
    Left error -> putStrLn error
    Right link -> do
        if not running
            then startServer >> callHtml link
            else callHtml link
    where callHtml link = do
            threadDelay 1000
            let name        = stripTemp $ last $ splitOn "/" fileName
                encodedName = BS.unpack $ urlEncode True $ BS.pack name
                encodedLink = BS.unpack $ urlEncode True $ BS.pack link
                sourcePath  = if vis then scorePath else visPath
                htmlURL = baseURL ++ "/" ++ sourcePath ++ sourceURL ++ encodedLink ++ nameURL ++ encodedName
            launch htmlURL