module Web.Pixiv.API
  ( -- * Trending
    getTrendingTags,
    getRecommendedIllusts,
    getRecommendedMangas,

    -- * Illust
    getIllustDetail,
    getIllustComments,
    getIllustRelated,
    getIllustRanking,
    getIllustFollow,
    getIllustNew,

    -- * Search
    searchIllust,

    -- * User
    getUserDetail,
    getUserIllusts,
    getUserFollowing,
    getUserFollower,
    getUserMypixiv,
  )
where

import Data.Coerce
import Data.Text (Text)
import qualified Web.Pixiv.API.Servant as S
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.PixivEntry (pageToOffset)
import Web.Pixiv.Types.PixivM
import Web.Pixiv.Types.Search

-----------------------------------------------------------------------------

getTrendingTags :: Maybe Bool -> PixivM [TrendingTag]
getTrendingTags includeTranslatedTagResults = do
  token <- getAccessToken
  tags <- liftC $ S.getTrendingTags token includeTranslatedTagResults
  pure $ coerce tags

getRecommendedIllusts :: Maybe Bool -> Maybe Bool -> PixivM [Illust]
getRecommendedIllusts includePrivacyPolicy includeTranslatedTagResults = do
  token <- getAccessToken
  illusts <- liftC $ S.getRecommendedIllusts token includePrivacyPolicy includeTranslatedTagResults
  pure $ coerce illusts

getRecommendedMangas :: Maybe Bool -> Maybe Bool -> PixivM [Illust]
getRecommendedMangas includePrivacyPolicy includeTranslatedTagResults = do
  token <- getAccessToken
  illusts <- liftC $ S.getRecommendedMangas token includePrivacyPolicy includeTranslatedTagResults
  pure $ coerce illusts

-----------------------------------------------------------------------------

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

getIllustRanking :: Maybe RankMode -> Int -> PixivM [Illust]
getIllustRanking mode (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.getIllustRanking token mode offset
  pure $ coerce illusts

getIllustFollow :: Maybe Publicity -> Int -> PixivM [Illust]
getIllustFollow restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.getIllustFollow token restrict offset
  pure $ coerce illusts

getIllustNew :: Int -> PixivM [Illust]
getIllustNew (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.getIllustNew token offset
  pure $ coerce illusts

-----------------------------------------------------------------------------

searchIllust ::
  SearchTarget ->
  Text ->
  Maybe Bool ->
  Maybe SortingMethod ->
  Maybe Duration ->
  Int ->
  PixivM [Illust]
searchIllust searchTarget searchWord includeTranslatedTag sortingMethod duration (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- liftC $ S.searchIllust token searchTarget searchWord includeTranslatedTag sortingMethod duration offset
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

getUserFollowing :: Int -> Maybe Publicity -> Int -> PixivM [UserPreview]
getUserFollowing userId restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- liftC $ S.getUserFollowing token userId restrict offset
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
