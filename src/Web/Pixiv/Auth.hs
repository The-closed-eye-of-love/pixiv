{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Web.Pixiv.Auth where

import Crypto.Hash.MD5
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import Data.Time
import Deriving.Aeson.Stock
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS

clientId :: ByteString
clientId = "MOBrBDS8blbauoSck0ZfDbtuzpyT"

clientSecret :: ByteString
clientSecret = "lsACyCD94FhDUtGTXi3QzcFE2uU1hqtDaKeqrdwj"

hashSecret :: ByteString
hashSecret = "28c1fdd170a5204386cb1313c7077b34f83e4aaf4aa829ce78c231e05b0bae2c"

data Credential = Credential
  { username :: ByteString,
    password :: ByteString
  }
  deriving (Show, Eq, Generic)

data OAuth2Token = OAuth2Token
  { accessToken :: Text,
    expiresIn :: Int,
    refreshToken :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake OAuth2Token

auth :: Credential -> IO (Maybe OAuth2Token)
auth Credential {..} = do
  manager <- newTlsManager
  let authUrl = "https://oauth.secure.pixiv.net/auth/token"
  initReq <- parseRequest authUrl
  utcT <- getCurrentTime
  let clientTime = C.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%I:%S+00:00" utcT
      clientHash = encode $ hash $ clientTime <> hashSecret
      headers =
        [ ("User-Agent", "PixivAndroidApp/5.0.64 (Android 6.0)"),
          ("X-Client-Time", clientTime),
          ("X-Client-Hash", clientHash)
        ]
      req = initReq {requestHeaders = headers}
      parts =
        [ partBS "client_id" clientId,
          partBS "client_secret" clientSecret,
          partBS "get_secure_url" "1",
          partBS "username" username,
          partBS "password" password,
          partBS "grant_type" "password"
        ]
  finalReq <- formDataBody parts req
  resp <- httpLbs finalReq manager
  let body = responseBody resp
  pure $ A.decode body
