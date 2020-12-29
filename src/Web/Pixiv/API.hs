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
    getUgoiraMetadata,

    -- * Search
    searchIllust,
    searchUser,

    -- * User
    getUserDetail,
    getUserIllusts,
    getUserFollowing,
    getUserFollower,
    getUserMypixiv,
    getUserBookmarks,
  )
where

import Control.Lens
import Data.Coerce (coerce)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Servant.Client.Core
import Web.Pixiv.API.Illust
import Web.Pixiv.API.PixivEntry (pageToOffset)
import Web.Pixiv.API.Search
import Web.Pixiv.API.Trending
import Web.Pixiv.API.User
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivT

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
  pure $ unNextUrl illusts

getRecommendedMangas :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedMangas includePrivacyPolicy includeTranslatedTagResults = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetRecommendedMangas) Proxy token includePrivacyPolicy includeTranslatedTagResults
  pure $ unNextUrl illusts

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
  pure $ unNextUrl illusts

getIllustRanking :: MonadPixiv m => Maybe RankMode -> Int -> m [Illust]
getIllustRanking mode (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustRanking) Proxy token mode offset
  pure $ unNextUrl illusts

getIllustFollow :: MonadPixiv m => Publicity -> Int -> m [Illust]
getIllustFollow restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustFollow) Proxy token restrict offset
  pure $ unNextUrl illusts

getIllustNew :: MonadPixiv m => Int -> m [Illust]
getIllustNew (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetIllustNew) Proxy token offset
  pure $ unNextUrl illusts

getUgoiraMetadata :: MonadPixiv m => Int -> m UgoiraMetadata
getUgoiraMetadata illustId = do
  token <- getAccessToken
  ug <- clientIn (Proxy @GetUgoiraMetadata) Proxy token illustId
  pure $ coerce ug

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
  pure $ unNextUrl illusts

searchUser ::
  MonadPixiv m =>
  Text ->
  Maybe SortingMethod ->
  Int ->
  m [UserPreview]
searchUser searchWord sortingMethod (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @SearchUser) Proxy token searchWord sortingMethod offset
  pure $ unNextUrl ups

-----------------------------------------------------------------------------

getUserDetail :: MonadPixiv m => Int -> m UserDetail
getUserDetail userId = do
  token <- getAccessToken
  clientIn (Proxy @GetUserDetail) Proxy token userId

getUserIllusts :: MonadPixiv m => Int -> Maybe IllustType -> Int -> m [Illust]
getUserIllusts userId illustType (pageToOffset -> offset) = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetUserIllusts) Proxy token userId illustType offset
  pure $ unNextUrl illusts

getUserFollowing :: MonadPixiv m => Int -> Publicity -> Int -> m [UserPreview]
getUserFollowing userId restrict (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserFollowing) Proxy token userId restrict offset
  pure $ unNextUrl ups

getUserFollower :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserFollower userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserFollower) Proxy token userId offset
  pure $ unNextUrl ups

getUserMypixiv :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserMypixiv userId (pageToOffset -> offset) = do
  token <- getAccessToken
  ups <- clientIn (Proxy @GetUserMypixiv) Proxy token userId offset
  pure $ unNextUrl ups

getUserBookmarks :: MonadPixiv m => Int -> Publicity -> Maybe Int -> m ([Illust], Maybe Int)
getUserBookmarks userId restrict maxBookmarkId = do
  token <- getAccessToken
  illusts <- clientIn (Proxy @GetUserBookmarks) Proxy token userId restrict maxBookmarkId
  pure (unNextUrl illusts, getNextUrl illusts >>= ((^? _Right . _1) . decimal . T.takeWhileEnd (/= '=')))
