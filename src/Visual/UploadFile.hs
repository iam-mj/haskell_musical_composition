module Visual.UploadFile where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text as T

type Error    = String
type MaybeURL = Either Error String 

fileHost   = "https://litterbox.catbox.moe/resources/internals/api.php" :: String

reqKey      = T.pack "reqtype"      :: T.Text
fileKey     = T.pack "fileToUpload" :: T.Text
expiracyKey = T.pack "time"         :: T.Text

reqType    = BS.pack "fileupload" :: BS.ByteString
expiration = BS.pack "24h"        :: BS.ByteString

errorMessage = "Error: failed to upload file to host, status = "


uploadRequest :: FilePath -> IO MaybeURL
uploadRequest fileName = do
  manager <- newManager tlsManagerSettings

  -- build the request
  initialRequest <- parseRequest $ "POST " ++ fileHost
  let parts = [partFile fileKey fileName, partBS expiracyKey expiration, partBS reqKey reqType]
  request <- formDataBody parts initialRequest

  -- handle response
  response <- httpLbs request manager
  let status = statusCode $ responseStatus response
  if status /= 200 
    then return $ Left $ errorMessage ++ show status
    else let link       = T.strip $ T.pack $ BLI.unpackChars $ responseBody response 
             linkString = T.unpack link
         in return $ Right linkString