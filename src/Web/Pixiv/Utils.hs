module Web.Pixiv.Utils
  ( mkDefaultClientEnv,
    runWithDefaultClientEnv,
    isSinglePageIllust,
    extractHighestQualityImageUrl,
    extractImageUrlsFromIllust,
  )
where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
import Web.Pixiv.Types
import Web.Pixiv.Types.Lens

mkDefaultClientEnv :: Manager -> IO ClientEnv
mkDefaultClientEnv manager = do
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

runWithDefaultClientEnv :: ClientM a -> IO (Either ClientError a)
runWithDefaultClientEnv m = newTlsManager >>= mkDefaultClientEnv >>= runClientM m

-----------------------------------------------------------------------------

isSinglePageIllust :: Illust -> Bool
isSinglePageIllust i = null $ i ^. metaPages

extractHighestQualityImageUrl :: ImageUrls -> Maybe Text
extractHighestQualityImageUrl x =
  x ^. original
    <|> x ^. large
    <|> x ^. medium
    <|> x ^. squareMedium

extractImageUrlsFromIllust :: Illust -> [Text]
extractImageUrlsFromIllust i
  | isSinglePageIllust i = (single <|> (multi ^? _head)) ^.. each
  | otherwise = multi
  where
    single = join $ (i ^. metaSinglePage) & each %~ (^. originalImageUrl)
    multi = (i ^. metaPages ^.. each . imageUrls <&> extractHighestQualityImageUrl) ^.. each . _Just
