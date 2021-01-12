-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Authentication pixiv API. Users should not use logics in this module directly,
-- since "Web.Pixiv.Types.PixivT" takes over token management, providing user friendly operations.
module Web.Pixiv.Auth
  ( Token (..),
    Credential (..),
    OAuth2Token (..),
    OAuth2Error (..),
    OAuth2Result (..),
    Errors (..),
    auth,
    auth',
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Base
import Crypto.Hash.MD5 (hash)
import Data.Aeson
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import Data.Generically
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData (PartM, formDataBody, partBS)

clientId :: ByteString
clientId = "MOBrBDS8blbauoSck0ZfDbtuzpyT"

clientSecret :: ByteString
clientSecret = "lsACyCD94FhDUtGTXi3QzcFE2uU1hqtDaKeqrdwj"

hashSecret :: ByteString
hashSecret = "28c1fdd170a5204386cb1313c7077b34f83e4aaf4aa829ce78c231e05b0bae2c"

-- | A wrapped 'Text' represents a token.
newtype Token = Token {unToken :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[UnwrapUnaryRecords] Token

-- | Authentication credentials for pixiv API.
--
-- Normally, users are supposed to create value of this data type using 'Password' constructor,
-- then pass it to 'Web.Pixiv.Types.PixivT.runPixivT'.
data Credential
  = Password
      { username :: ByteString,
        password :: ByteString
      }
  | RefreshToken
      { cr_refreshToken :: Token
      }
  deriving stock (Show, Eq)

mkAuthParts :: Applicative m => Credential -> [PartM m]
mkAuthParts Password {..} =
  [ partBS "grant_type" "password",
    partBS "username" username,
    partBS "password" password
  ]
mkAuthParts RefreshToken {..} =
  [ partBS "grant_type" "refresh_token",
    partBS "refresh_token" (encodeUtf8 . unToken $ cr_refreshToken)
  ]

-- | Successful result.
data OAuth2Token = OAuth2Token
  { oa_accessToken :: Token,
    oa_expiresIn :: Int,
    oa_refreshToken :: Token
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "oa_" OAuth2Token

-- | Authentication failure reasions.
data Errors
  = InvalidRequest
  | InvalidClient
  | InvalidGrant
  | UnauthorizedClient
  | UnsupportedGrantType
  | InvalidScope
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via EnumJSON' Errors

-- | Failed result.
data OAuth2Error = OAuth2Error
  { oa_error :: Errors,
    oa_message :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

instance FromJSON OAuth2Error where
  parseJSON = withObject "oauth2 response" $ \o -> do
    oa_error <- o .: "error"
    errors <- o .: "errors"
    oa_message <- flip (withObject "errors") errors $ \o' -> do
      system <- o' .: "system"
      flip (withObject "system") system $ \o'' -> do
        o'' .: "message"
    pure OAuth2Error {..}

-- | Authentication result.
data OAuth2Result
  = AuthSuccess OAuth2Token
  | AuthFailure OAuth2Error
  deriving stock (Show, Eq, Generic)

instance FromJSON OAuth2Result where
  parseJSON v =
    AuthSuccess <$> parseJSON v <|> AuthFailure <$> parseJSON v

-- | Given a credential, performs a authentication request.
auth :: Manager -> Credential -> IO OAuth2Result
auth manager credential = do
  let authUrl = "https://oauth.secure.pixiv.net/auth/token"
  initReq <- parseRequest authUrl
  utcT <- getCurrentTime
  let clientTime = C.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%I:%S+00:00" utcT
      clientHash = B16.encode $ hash $ clientTime <> hashSecret
      headers =
        [ ("User-Agent", "PixivAndroidApp/5.0.64 (Android 6.0)"),
          ("X-Client-Time", clientTime),
          ("X-Client-Hash", clientHash)
        ]
      req = initReq {requestHeaders = headers}
      parts =
        [ partBS "client_id" clientId,
          partBS "client_secret" clientSecret,
          partBS "get_secure_url" "1"
        ]
          ++ mkAuthParts credential
  finalReq <- formDataBody parts req
  resp <- httpLbs finalReq manager
  let body = responseBody resp
  maybe (fail "impossible: unable to parse response") pure (A.decode body)

-- | Like 'auth', but immediately throws 'OAuth2Error' if auth failed.
auth' :: Manager -> Credential -> IO OAuth2Token
auth' manager credential =
  auth manager credential >>= \case
    AuthSuccess t -> pure t
    AuthFailure err -> throwIO err
