module Web.Pixiv.Utils
  ( mkDefaultClientEnv,
    runWithDefaultClientEnv,
    isSinglePageIllust,
    extractHighestQualityImageUrl,
    extractImageUrlsFromIllust,
    unzipArchive,
    ugoiraMetadataToFFConcat,
  )
where

import qualified Codec.Archive.Zip as Zip
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Numeric (showFFloat)
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

-----------------------------------------------------------------------------

unzipArchive :: FilePath -> LBS.ByteString -> IO ()
unzipArchive dir bs = do
  let archive = Zip.toArchive bs
      option = [Zip.OptDestination dir]
  Zip.extractFilesFromArchive option archive

ugoiraMetadataToFFConcat :: UgoiraMetadata -> BS.ByteString
ugoiraMetadataToFFConcat meta =
  encodeUtf8 . T.unlines $
    "ffconcat version 1.0" :
      [ T.unlines
          [ "file " <> frame ^. ugoiraFile,
            "duration " <> T.pack (frame ^. ugoiraDelay & fromIntegral & (/ 1000) & flip (showFFloat @Double Nothing) "")
          ]
        | frame <- meta ^. frames
      ]