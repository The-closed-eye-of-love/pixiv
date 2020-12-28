module Web.Pixiv.Download
  ( downloadSingleIllust,
    downloadSingleIllust',
  )
where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import Network.HTTP.Client
import Web.Pixiv.Types
import Web.Pixiv.Types.Lens
import Web.Pixiv.Types.PixivT

-- | Download a single page illust.
-- Choose the first one if the illust has many pages.
-- Prefer high quality images.
downloadSingleIllust :: Manager -> Illust -> IO (Maybe ByteString)
downloadSingleIllust manager i = do
  let single = join $ (i ^. metaSinglePage) & each %~ (^. originalImageUrl)
      multiFirst =
        join $
          i ^. metaPages ^? _head
            & each %~ (^. imageUrls)
            & each %~ (\x -> x ^. original <|> x ^. large <|> x ^. medium <|> x ^. squareMedium)
      addReferer r = r {requestHeaders = [("Referer", "https://app-api.pixiv.net/")]}
  req <- (parseRequest . T.unpack) `traverse` (single <|> multiFirst)
  case req of
    Just (addReferer -> req') -> do
      resp <- httpLbs req' manager
      pure . pure . toStrict $ responseBody resp
    _ -> pure Nothing

-- | Same as 'downloadSingleIllust', but use 'Manager' in 'TokenState'.
downloadSingleIllust' :: (MonadIO m) => Illust -> PixivT m (Maybe ByteString)
downloadSingleIllust' i = do
  m <- manager <$> takeTokenState
  liftIO $ downloadSingleIllust m i
