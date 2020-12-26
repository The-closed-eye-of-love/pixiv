{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Aeson
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

testDecode :: forall v. (FromJSON v) => FilePath -> IO ()
testDecode path =
  eitherDecodeFileStrict @v path >>= \case
    Left err -> fail err
    Right _ -> putStrLn "Pass"
