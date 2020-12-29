{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString as BS
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

  pillust <- eitherDecodeFileStrict @IllustWrapper "test/illust_detail.json"
  case pillust of
    Left err -> fail err
    Right IllustWrapper {..} -> do
      manager <- newTlsManager
      mresult <- downloadSingleIllust manager _illust
      case mresult of
        Just result -> BS.writeFile "temp.jpg" result
        _ -> fail "Failed to download"

testDecode :: forall v. (FromJSON v) => FilePath -> IO ()
testDecode path =
  eitherDecodeFileStrict @v path >>= \case
    Left err -> fail err
    Right _ -> putStrLn "Pass"
