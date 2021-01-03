{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.TLS (newTlsManager)
import Web.Pixiv.Download
import Web.Pixiv.Types

main :: IO ()
main = do
  testDecode @Comments "test/illust_comments.json"
  testDecode @Illusts "test/illust_related.json"
  testDecode @UserDetail "test/user_detail.json"
  testDecode @Illusts "test/user_illusts.json"
  testDecode @TrendingTags "test/trending_tags.json"
  testDecode @Illusts "test/search_illust.json"
  testDecode @UserPreviews "test/user_follower.json"
  testDecode @UserPreviews "test/user_following.json"
  testDecode @UserPreviews "test/user_mypixiv.json"

  manager <- newTlsManager

  pillust <- eitherDecodeFileStrict @IllustWrapper "test/illust_detail.json"
  case pillust of
    Left err -> fail err
    Right IllustWrapper {..} -> do
      mresult <- runDownloadM manager $ downloadSingleIllust _illust
      case mresult of
        Just result -> LBS.writeFile "temp.jpg" result >> putStrLn "Write jpg"
        _ -> fail "Failed to download"
  pmetadata <- eitherDecodeFileStrict @UgoiraMetadataWrapper "test/ugoira_metadata.json"
  case pmetadata of
    Left err -> fail err
    Right UgoiraMetadataWrapper {..} -> do
      mresult <- runDownloadM manager $ downloadUgoiraToMP4 _ugoiraMetadata Nothing
      case mresult of
        Just result -> LBS.writeFile "temp.mp4" result >> putStrLn "Write mp4"
        _ -> fail "Failed to download"

testDecode :: forall v. (FromJSON v) => FilePath -> IO ()
testDecode path =
  eitherDecodeFileStrict @v path >>= \case
    Left err -> fail err
    Right _ -> putStrLn "Pass"
