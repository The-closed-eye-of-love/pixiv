module Web.Pixiv.Download
  ( DownloadM,
    liftMaybe,
    liftToPixivT,
    runDownloadM,
    downloadPixiv,
    downloadSingleIllust,
    downloadUgoiraToMP4,
  )
where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process
import Web.Pixiv.Types
import Web.Pixiv.Types.Lens
import Web.Pixiv.Types.PixivT
import Web.Pixiv.Utils

type DownloadM = MaybeT (ReaderT Manager IO)

liftMaybe :: Maybe a -> DownloadM a
liftMaybe x = MaybeT . liftIO $ pure x

runDownloadM :: Manager -> DownloadM a -> IO (Maybe a)
runDownloadM manager m = runMaybeT m & flip runReaderT manager

liftToPixivT :: (MonadIO m) => DownloadM a -> PixivT m (Maybe a)
liftToPixivT m = do
  PixivState {tokenState = TokenState {..}} <- readPixivState
  liftIO $ runDownloadM manager m

downloadPixiv :: Text -> DownloadM LBS.ByteString
downloadPixiv url = do
  let addReferer r = r {requestHeaders = [("Referer", "https://app-api.pixiv.net/")]}
  manager <- ask
  req <- addReferer <$> (parseRequest . T.unpack $ url)
  resp <- liftIO $ httpLbs req manager
  pure $ responseBody resp

-- | Download a single page illust.
-- Choose the first one if the illust has many pages.
-- Prefer high quality images. Returns `Nothing` if can't find image url.
downloadSingleIllust :: Illust -> DownloadM LBS.ByteString
downloadSingleIllust i = do
  url <- liftMaybe $ extractImageUrlsFromIllust i ^? _head
  downloadPixiv url

downloadUgoiraToMP4 :: UgoiraMetadata -> Maybe FilePath -> DownloadM (String, LBS.ByteString)
downloadUgoiraToMP4 meta (fromMaybe "ffmpeg" -> ffmpeg) = do
  let ffconcat = ugoiraMetadataToFFConcat meta
  bs0 <- downloadPixiv $ meta ^. zipUrls . zipMedium
  systmp <- liftIO getCanonicalTemporaryDirectory
  liftIO $
    withTempDirectory systmp "ugoira" $ \temp -> do
      unzipArchive temp bs0
      let concatFilePath = temp </> "concat"
          convertProcess =
            ( proc
                ffmpeg
                [ "-y",
                  "-i",
                  "concat",
                  "-c:v",
                  "libx264",
                  "-vf",
                  "pad=ceil(iw/2)*2:ceil(ih/2)*2",
                  "-pix_fmt",
                  "yuv420p",
                  "-lossless",
                  "1",
                  "ugoira.mp4"
                ]
            )
              { cwd = Just temp
              }
      BS.writeFile concatFilePath ffconcat
      (_code, _stdout, stderr) <- readCreateProcessWithExitCode convertProcess ""
      (stderr,) <$> LBS.readFile (temp </> "ugoira.mp4")