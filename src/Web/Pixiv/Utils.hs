module Web.Pixiv.Utils where

import Network.HTTP.Client.TLS
import Servant.Client

mkDefaultClientEnv :: IO ClientEnv
mkDefaultClientEnv = do
  manager <- newTlsManager
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

runWithDefaultClientEnv :: ClientM a -> IO (Either ClientError a)
runWithDefaultClientEnv m = mkDefaultClientEnv >>= runClientM m
