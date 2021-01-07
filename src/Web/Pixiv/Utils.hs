-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Miscellaneous utilities.
module Web.Pixiv.Utils
  ( isSinglePageIllust,
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
import Numeric (showFFloat)
import Web.Pixiv.Types
import Web.Pixiv.Types.Lens

-----------------------------------------------------------------------------

-- | Judges if an illustration has only one image.
isSinglePageIllust :: Illust -> Bool
isSinglePageIllust i = null $ i ^. metaPages

-- | Extracts the url of the highest quality image.
extractHighestQualityImageUrl :: ImageUrls -> Maybe Text
extractHighestQualityImageUrl x =
  x ^. original
    <|> x ^. large
    <|> x ^. medium
    <|> x ^. squareMedium

-- | Extracts all urls from an illustration, using 'extractHighestQualityImageUrl'.
extractImageUrlsFromIllust :: Illust -> [Text]
extractImageUrlsFromIllust i
  | isSinglePageIllust i = (single <|> (multi ^? _head)) ^.. each
  | otherwise = multi
  where
    single = join $ (i ^. metaSinglePage) & each %~ (^. originalImageUrl)
    multi = (i ^. metaPages ^.. each . imageUrls <&> extractHighestQualityImageUrl) ^.. each . _Just

-----------------------------------------------------------------------------

-- | Unzip a zip archive represented in 'LBS.ByteString' to a directory.
unzipArchive ::
  -- | destination directory
  FilePath ->
  -- | zip archive
  LBS.ByteString ->
  IO ()
unzipArchive dir bs = do
  let archive = Zip.toArchive bs
      option = [Zip.OptDestination dir]
  Zip.extractFilesFromArchive option archive

-- | Generates ffconcat meta file of an ugoira.
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
