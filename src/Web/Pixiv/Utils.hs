module Web.Pixiv.Utils
  ( mkDefaultClientEnv,
    runWithDefaultClientEnv,
    PixivJSON,
    PixivJSON',
    EnumJSON,
    EnumJSON',
    PixivLabelModifier,
  )
where

import Data.Data (Proxy (Proxy))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Deriving.Aeson
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client

mkDefaultClientEnv :: Manager -> IO ClientEnv
mkDefaultClientEnv manager = do
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

runWithDefaultClientEnv :: ClientM a -> IO (Either ClientError a)
runWithDefaultClientEnv m = newTlsManager >>= mkDefaultClientEnv >>= runClientM m

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
