module Visual.UploadFile where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.Header (hUserAgent)
import qualified Data.Text as T

fileHost   = "https://0x0.st" :: String

fileKey     = T.pack "file"    :: T.Text
expiracyKey = T.pack "expires" :: T.Text

expiration = BS.pack "2"           :: BS.ByteString -- 2 hours
userAgent  = BS.pack "curl/7.68.0" :: BS.ByteString


uploadRequest :: FilePath -> IO ()
uploadRequest fileName = do
  manager <- newManager tlsManagerSettings

  -- build the request
  initialRequest <- parseRequest $ "POST " ++ fileHost
  let requestHeaders = initialRequest {requestHeaders = [(hUserAgent, userAgent)]}
      parts          = [partFile fileKey fileName, partBS expiracyKey expiration]
  request  <- formDataBody parts requestHeaders

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (responseStatus response)
  print $ responseBody response