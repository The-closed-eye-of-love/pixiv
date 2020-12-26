module Web.Pixiv.API where

import Data.Coerce
import Data.Text (Text)
import qualified Web.Pixiv.API.Servant as S
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.PixivM
import Web.Pixiv.Types.Search

getTrendingTags :: PixivM TrendingTags
getTrendingTags = getAccessToken >>= (liftC . S.getTrendingTags)

getIllustDetail :: Int -> PixivM Illust
getIllustDetail illustId = do
  token <- getAccessToken
  detail <- liftC $ S.getIllustDetail token illustId
  pure $ coerce detail

getIllustComments :: Int -> PixivM Comments
getIllustComments illustId = do
  token <- getAccessToken
  liftC $ S.getIllustComments token illustId

getIllustRelated :: Int -> PixivM [Illust]
getIllustRelated illustId = do
  token <- getAccessToken
  illusts <- liftC $ S.getIllustRelated token illustId
  pure $ coerce illusts

searchIllust ::
  Text ->
  Int ->
  Maybe Bool ->
  Maybe SortingMethod ->
  Maybe Duration ->
  PixivM [Illust]
searchIllust searchString page includeTranslatedTag sortingMethod duration = do
  token <- getAccessToken
  illusts <- liftC $ S.searchIllust token searchString page includeTranslatedTag sortingMethod duration
  pure $ coerce illusts

getUserDetail :: Int -> PixivM UserDetail
getUserDetail userId = do
  token <- getAccessToken
  liftC $ S.getUserDetail token userId

getUserIllusts :: Int -> PixivM [Illust]
getUserIllusts userId = do
  token <- getAccessToken
  illusts <- liftC $ S.getUserIllusts token userId
  pure $ coerce illusts

getUserFollowing :: Int -> PixivM [UserPreview]
getUserFollowing userId = do
  token <- getAccessToken
  ups <- liftC $ S.getUserFollowing token userId
  pure $ coerce ups

getUserFollower :: Int -> PixivM [UserPreview]
getUserFollower userId = do
  token <- getAccessToken
  ups <- liftC $ S.getUserFollower token userId
  pure $ coerce ups

getUserMypixiv :: Int -> PixivM [UserPreview]
getUserMypixiv userId = do
  token <- getAccessToken
  ups <- liftC $ S.getUserMypixiv token userId
  pure $ coerce ups
