module Web.Pixiv.Types.PixivEntry where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API hiding (addHeader)
import Servant.Client.Core
import Web.Pixiv.Auth (Token (..))

data PixivEntry

instance HasClient m api => HasClient m (PixivEntry :> api) where
  type Client m (PixivEntry :> api) = Token -> Client m api
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

type OffsetParam = QueryParam "offset" Int

pageToOffset :: Int -> Maybe Int
pageToOffset x
  | x > 0 = Just $ (x - 1) * 30
  | otherwise = Nothing
