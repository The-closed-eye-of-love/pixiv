module Web.Pixiv.Utils where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client

mkDefaultClientEnv :: Manager -> IO ClientEnv
mkDefaultClientEnv manager = do
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

runWithDefaultClientEnv :: ClientM a -> IO (Either ClientError a)
runWithDefaultClientEnv m = newTlsManager >>= mkDefaultClientEnv >>= runClientM m
