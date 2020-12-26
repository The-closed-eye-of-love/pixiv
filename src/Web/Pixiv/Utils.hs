module Web.Pixiv.Utils where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Deriving.Aeson
import GHC.TypeLits
import Network.HTTP.Client.TLS
import Servant.Client

mkDefaultClientEnv :: IO ClientEnv
mkDefaultClientEnv = do
  manager <- newTlsManager
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

runWithDefaultClientEnv :: ClientM a -> IO (Either ClientError a)
runWithDefaultClientEnv m = mkDefaultClientEnv >>= runClientM m

-----------------------------------------------------------------------------

type PixivJSON (k :: Symbol) = CustomJSON '[FieldLabelModifier (PixivLabelModifier k, CamelToSnake)]

type EnumJSON (k :: Symbol) = CustomJSON '[ConstructorTagModifier (StripPrefix k, CamelToSnake)]

type PixivJSON' = PixivJSON ""

type EnumJSON' = EnumJSON ""

-- | Strip prefix @'_'@ and @k@, making sure the result is non-empty
data PixivLabelModifier (k :: Symbol)

instance KnownSymbol k => StringModifier (PixivLabelModifier k) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @k)) s' of
    Nothing -> s'
    Just "" -> s'
    Just x -> x
    where
      s' = fromMaybe s (stripPrefix "_" s)
