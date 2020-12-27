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
import Data.Proxy
import Data.Text (Text)
import Servant.Client.Core
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.PixivEntry (pageToOffset)
import Web.Pixiv.Types.PixivT
import Web.Pixiv.Types.Search
import Web.Pixiv.Types.Trending
import Web.Pixiv.Types.User

-----------------------------------------------------------------------------

getTrendingTags :: MonadPixiv m => Maybe Bool -> m [TrendingTag]
getTrendingTags includeTranslatedTagResults = do
  token <- getAccessToken
  tags <- clientIn (Proxy @GetTrendingTags) Proxy token includeTranslatedTagResults
  pure $ coerce tags

getRecommendedIllusts :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedIllusts includePrivacyPolicy includeTranslatedTagResults = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetRecommendedIllusts) Proxy token includePrivacyPolicy includeTranslatedTagResults
  pure $ coerce illusts

getRecommendedMangas :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedMangas includePrivacyPolicy includeTranslatedTagResults = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetRecommendedMangas) Proxy token includePrivacyPolicy includeTranslatedTagResults
  pure $ coerce illusts

-----------------------------------------------------------------------------

getIllustDetail :: MonadPixiv m => Int -> m Illust
getIllustDetail illustId = do
  token <- getAccessToken
  detail <- clientIn (Proxy @GetIllustDetail) Proxy token illustId
  pure $ coerce detail

getIllustComments :: MonadPixiv m => Int -> Int -> m Comments
getIllustComments illustId (pageToOffset -> offset) = do
  token <- getAccessToken
  clientIn (Proxy @GetIllustComments) Proxy token illustId offset

getIllustRelated :: MonadPixiv m => Int -> Int -> m [Illust]
getIllustRelated illustId (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustRelated) Proxy token illustId offset
  pure $ coerce illusts

getIllustRanking :: MonadPixiv m => Maybe RankMode -> Int -> m [Illust]
getIllustRanking mode (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustRanking) Proxy token mode offset
  pure $ coerce illusts

getIllustFollow :: MonadPixiv m => Maybe Publicity -> Int -> m [Illust]
getIllustFollow restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustFollow) Proxy token restrict offset
  pure $ coerce illusts

getIllustNew :: MonadPixiv m => Int -> m [Illust]
getIllustNew (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustNew) Proxy token offset
  pure $ coerce illusts

-----------------------------------------------------------------------------

searchIllust ::
  MonadPixiv m =>
  SearchTarget ->
  Text ->
  Maybe Bool ->
  Maybe SortingMethod ->
  Maybe Duration ->
  Int ->
  m [Illust]
searchIllust searchTarget searchWord includeTranslatedTag sortingMethod duration (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @SearchIllust) Proxy token searchTarget searchWord includeTranslatedTag sortingMethod duration offset
  pure $ coerce illusts

getUserDetail :: MonadPixiv m => Int -> m UserDetail
getUserDetail userId = do
  token <- getAccessToken
  clientIn (Proxy @GetUserDetail) Proxy token userId

getUserIllusts :: MonadPixiv m => Int -> Maybe IllustType -> Int -> m [Illust]
getUserIllusts userId illustType (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetUserIllusts) Proxy token userId illustType offset
  pure $ coerce illusts

getUserFollowing :: MonadPixiv m => Int -> Maybe Publicity -> Int -> m [UserPreview]
getUserFollowing userId restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserFollowing) Proxy token userId restrict offset
  pure $ coerce ups

getUserFollower :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserFollower userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserFollower) Proxy token userId offset
  pure $ coerce ups

getUserMypixiv :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserMypixiv userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserMypixiv) Proxy token userId offset
  pure $ coerce ups
