module Web.Pixiv.Types.Pixiv where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API hiding (addHeader)
import Servant.Client.Core
import Web.Pixiv.Auth (Token (..))

data Pixiv

instance HasClient m api => HasClient m (Pixiv :> api) where
  type Client m (Pixiv :> api) = Token -> Client m api
  clientWithRoute pm Proxy req = \(unToken -> token) ->
    clientWithRoute pm (Proxy @api) $
      req
        & addHeader @Text "App-OS" "ios"
        & addHeader @Text "App-OS-Version" "12.2"
        & addHeader @Text "App-Version" "7.6.2"
        & addHeader @Text "User-Agent" "PixivIOSApp/7.6.2 (iOS 12.2; iPhone9,1)"
        & addHeader @Text "Authorization" ("Bearer " <> token)
  hoistClientMonad pm Proxy f m token =
    hoistClientMonad pm (Proxy @api) f (m token)
