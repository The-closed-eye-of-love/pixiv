module Web.Pixiv.API where

import Data.Coerce
import Data.Text (Text)
import qualified Web.Pixiv.API.Servant as S
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.PixivEntry (pageToOffset)
import Web.Pixiv.Types.PixivM
import Web.Pixiv.Types.Search

getTrendingTags :: PixivM TrendingTags
getTrendingTags = getAccessToken >>= (liftC . S.getTrendingTags)

getIllustDetail :: Int -> PixivM Illust
getIllustDetail illustId = do
  token <- getAccessToken
  detail <- liftC $ S.getIllustDetail token illustId
  pure $ coerce detail

getIllustComments :: Int -> Int -> PixivM Comments
getIllustComments illustId (pageToOffset -> offset) = do
  token <- getAccessToken
  liftC $ S.getIllustComments token illustId offset

getIllustRelated :: Int -> Int -> PixivM [Illust]
getIllustRelated illustId (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.getIllustRelated token illustId offset
  pure $ coerce illusts

searchIllust ::
  Text ->
  Maybe Bool ->
  Maybe SortingMethod ->
  Maybe Duration ->
  Int ->
  PixivM [Illust]
searchIllust searchString includeTranslatedTag sortingMethod duration (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.searchIllust token searchString includeTranslatedTag sortingMethod duration offset
  pure $ coerce illusts

getUserDetail :: Int -> PixivM UserDetail
getUserDetail userId = do
  token <- getAccessToken
  liftC $ S.getUserDetail token userId

getUserIllusts :: Int -> Maybe IllustType -> Int -> PixivM [Illust]
getUserIllusts userId illustType (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.getUserIllusts token userId illustType offset
  pure $ coerce illusts

getUserFollowing :: Int -> Int -> PixivM [UserPreview]
getUserFollowing userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- liftC $ S.getUserFollowing token userId offset
  pure $ coerce ups

getUserFollower :: Int -> Int -> PixivM [UserPreview]
getUserFollower userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- liftC $ S.getUserFollower token userId offset
  pure $ coerce ups

getUserMypixiv :: Int -> Int -> PixivM [UserPreview]
getUserMypixiv userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- liftC $ S.getUserMypixiv token userId offset
  pure $ coerce ups
