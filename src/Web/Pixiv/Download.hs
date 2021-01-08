-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- A set of utilities for downloading pixiv things.
module Web.Pixiv.Download
  ( -- * DownloadM monad
    DownloadM,
    liftMaybe,
    liftToPixivT,
    runDownloadM,

    -- * Download actions
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

-- | 'DownloadM' monad is a synonym for 'IO' computation wrapped in 'Manager' environment,
-- which may exit without producing value, indicating the download failed.
type DownloadM = MaybeT (ReaderT Manager IO)

-- | Lifts a pure 'Maybe' value to 'DownloadM'.
liftMaybe :: Maybe a -> DownloadM a
liftMaybe x = MaybeT . liftIO $ pure x

-- | Executes the download computation in 'IO'.
runDownloadM :: Manager -> DownloadM a -> IO (Maybe a)
runDownloadM manager m = runMaybeT m & flip runReaderT manager

-- | Lifts a download computation to 'PixivT'.
--
-- 'DownloadM' needs 'Manager' to perform the download, which can be provided by 'TokenState'.
liftToPixivT :: (MonadIO m) => DownloadM a -> PixivT m (Maybe a)
liftToPixivT m = do
  PixivState {tokenState = TokenState {..}} <- readPixivState
  liftIO $ runDownloadM manager m

-- | Downloads something in 'DownloadM', given url.
downloadPixiv :: Text -> DownloadM LBS.ByteString
downloadPixiv url = do
  let addReferer r = r {requestHeaders = [("Referer", "https://app-api.pixiv.net/")]}
  manager <- ask
  req <- addReferer <$> (parseRequest . T.unpack $ url)
  resp <- liftIO $ httpLbs req manager
  pure $ responseBody resp

-- | Downloads a single page illust.
--
-- Chooses the first one if the illust has many pages,
-- preferring high quality images. Returns `Nothing` if can't find any image url.
downloadSingleIllust :: Illust -> DownloadM LBS.ByteString
downloadSingleIllust i = do
  url <- liftMaybe $ extractImageUrlsFromIllust i ^? _head
  downloadPixiv url

-- | Downloads 'UgoiraFrame's, then converts it to MP4 calling external @ffmpeg@.
downloadUgoiraToMP4 ::
  -- | Information of ugoira to download
  UgoiraMetadata ->
  -- | Path to @ffmpeg@
  Maybe FilePath ->
  DownloadM (String, LBS.ByteString)
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
