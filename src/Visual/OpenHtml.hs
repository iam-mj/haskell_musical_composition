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
import Network.HTTP.Types (urlEncode)
import qualified Data.ByteString.Char8 as BS

serverPort :: Int
serverPort    = 3000
staticFolder  = "src/static"
htmlPath      = "index.html"
baseURL       = "http://localhost:" ++ show serverPort
sourceURL     = "?src="
nameURL       = "&name="

-- note: scotty port is blocking, so we start the server in a different thread
startServer :: IO ()
startServer = do
    void $ forkIO $ scotty serverPort $ do
        middleware $ staticPolicy (addBase staticFolder)
        get "/" $ file $ staticFolder ++ "/" ++ htmlPath

openHtml :: FilePath -> IO ()
openHtml fileName = do
    -- upload the file to a file host
  uploadResult <- uploadRequest fileName
  case uploadResult of
    Left error -> putStrLn error
    Right link -> do
        startServer
        threadDelay 1000
        let name        = last $ splitOn "/" fileName
            encodedName = BS.unpack $ urlEncode True $ BS.pack name
            encodedLink = BS.unpack $ urlEncode True $ BS.pack link
            htmlURL = baseURL ++ "/" ++ htmlPath ++ sourceURL ++ encodedLink ++ nameURL ++ encodedName
        callCommand $ "start \"\" \"" ++ htmlURL ++ "\""