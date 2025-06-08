module Visual.CreateScore where
    
import System.Process
import System.Info
import qualified Data.ByteString.Char8 as BS

import Visual.UploadFile
import Network.HTTP.Types

endpoint      = "https://www.noteflight.com/scores/create?scoreTemplateURL=" :: String
setImportType = "&scoreImportType=midi"                                      :: String

createScore :: FilePath -> IO ()
createScore fileName = do
  -- upload the file to a file host
  uploadResult <- uploadRequest fileName
  case uploadResult of
    Left error -> putStrLn error
    Right link -> do
        -- build launch link
        -- note: urlEncode's first argument is set to true => we encode a part of a query string
        let linkBS      = BS.pack link
            encodedLink = BS.unpack $ urlEncode True linkBS 
            launchLink  = endpoint ++ encodedLink ++ setImportType
        -- open the link in the default browser
        launch launchLink

-- launch an url depending on the os
launch :: String -> IO ()
launch launchLink = case os of
    "mingw32" -> callCommand $ "start \"\" \"" ++ launchLink ++ "\"" -- windows
    "darwin"  -> callCommand $ "open \"" ++ launchLink ++ "\""       -- macOS
    _         -> callCommand $ "xdg-open \"" ++ launchLink ++ "\""   -- linux