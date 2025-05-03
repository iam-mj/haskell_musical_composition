module Visual.CreateScore where
    
import System.Process
import qualified Data.ByteString.Char8 as BS

import Visual.UploadFile
import Network.HTTP.Types

endpoint      = "\"https://www.noteflight.com/scores/create?scoreTemplateURL=" :: String
setImportType = "&scoreImportType=midi\""                                      :: String

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
            launchLink  = endpoint ++ link ++ setImportType
        
        -- launch the score editor
        callCommand $ "start \"\" " ++ launchLink