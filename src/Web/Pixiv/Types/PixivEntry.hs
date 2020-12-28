module Web.Pixiv.Types.PixivEntry where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API (QueryParam, QueryParam', Required, Strict, type (:>))
import Servant.Client.Core (HasClient (..), addHeader, appendToQueryString)
import Web.Pixiv.Auth (Token (..))
import Web.Pixiv.Types (Publicity)

data PixivEntry

instance HasClient m api => HasClient m (PixivEntry :> api) where
  type Client m (PixivEntry :> api) = Token -> Client m api
  clientWithRoute pm Proxy req = \(unToken -> token) ->
    clientWithRoute pm (Proxy @api) $
      req
        & addHeader @Text "User-Agent" "PixivAndroidApp/5.0.175 (Android 6.0; PixivHaskell)"
        & addHeader @Text "Authorization" ("Bearer " <> token)
        & appendToQueryString "filter" (Just "for_android")
  hoistClientMonad pm Proxy f m token =
    hoistClientMonad pm (Proxy @api) f (m token)

type OffsetParam = QueryParam "offset" Int

pageToOffset :: Int -> Maybe Int
pageToOffset x
  | x > 1 = Just $ (x - 1) * 30
  | otherwise = Nothing

type RestrictParam = QueryParam' '[Strict, Required] "restrict" Publicity
