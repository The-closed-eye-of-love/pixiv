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
  p <- getAccessTokenWithAccpetLanguage
  tags <- clientIn (Proxy @GetTrendingTags) Proxy p includeTranslatedTagResults
  pure $ coerce tags

getRecommendedIllusts :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedIllusts includePrivacyPolicy includeTranslatedTagResults = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetRecommendedIllusts) Proxy p includePrivacyPolicy includeTranslatedTagResults
  pure $ unNextUrl illusts

getRecommendedMangas :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedMangas includePrivacyPolicy includeTranslatedTagResults = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetRecommendedMangas) Proxy p includePrivacyPolicy includeTranslatedTagResults
  pure $ unNextUrl illusts

-----------------------------------------------------------------------------

getIllustDetail :: MonadPixiv m => Int -> m Illust
getIllustDetail illustId = do
  p <- getAccessTokenWithAccpetLanguage
  detail <- clientIn (Proxy @GetIllustDetail) Proxy p illustId
  pure $ coerce detail

getIllustComments :: MonadPixiv m => Int -> Int -> m Comments
getIllustComments illustId (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  clientIn (Proxy @GetIllustComments) Proxy p illustId offset

getIllustRelated :: MonadPixiv m => Int -> Int -> m [Illust]
getIllustRelated illustId (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustRelated) Proxy p illustId offset
  pure $ unNextUrl illusts

getIllustRanking :: MonadPixiv m => Maybe RankMode -> Int -> m [Illust]
getIllustRanking mode (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustRanking) Proxy p mode offset
  pure $ unNextUrl illusts

getIllustFollow :: MonadPixiv m => Publicity -> Int -> m [Illust]
getIllustFollow restrict (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustFollow) Proxy p restrict offset
  pure $ unNextUrl illusts

getIllustNew :: MonadPixiv m => Int -> m [Illust]
getIllustNew (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustNew) Proxy p offset
  pure $ unNextUrl illusts

getUgoiraMetadata :: MonadPixiv m => Int -> m UgoiraMetadata
getUgoiraMetadata illustId = do
  p <- getAccessTokenWithAccpetLanguage
  ug <- clientIn (Proxy @GetUgoiraMetadata) Proxy p illustId
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
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @SearchIllust) Proxy p searchTarget searchWord includeTranslatedTag sortingMethod duration offset
  pure $ unNextUrl illusts

searchUser ::
  MonadPixiv m =>
  Text ->
  Maybe SortingMethod ->
  Int ->
  m [UserPreview]
searchUser searchWord sortingMethod (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @SearchUser) Proxy p searchWord sortingMethod offset
  pure $ unNextUrl ups

-----------------------------------------------------------------------------

getUserDetail :: MonadPixiv m => Int -> m UserDetail
getUserDetail userId = do
  p <- getAccessTokenWithAccpetLanguage
  clientIn (Proxy @GetUserDetail) Proxy p userId

getUserIllusts :: MonadPixiv m => Int -> Maybe IllustType -> Int -> m [Illust]
getUserIllusts userId illustType (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetUserIllusts) Proxy p userId illustType offset
  pure $ unNextUrl illusts

getUserFollowing :: MonadPixiv m => Int -> Publicity -> Int -> m [UserPreview]
getUserFollowing userId restrict (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserFollowing) Proxy p userId restrict offset
  pure $ unNextUrl ups

getUserFollower :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserFollower userId (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserFollower) Proxy p userId offset
  pure $ unNextUrl ups

getUserMypixiv :: MonadPixiv m => Int -> Int -> m [UserPreview]
getUserMypixiv userId (pageToOffset -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserMypixiv) Proxy p userId offset
  pure $ unNextUrl ups

getUserBookmarks :: MonadPixiv m => Int -> Publicity -> Maybe Int -> m ([Illust], Maybe Int)
getUserBookmarks userId restrict maxBookmarkId = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetUserBookmarks) Proxy p userId restrict maxBookmarkId
  pure (unNextUrl illusts, getNextUrl illusts >>= ((^? _Right . _1) . decimal . T.takeWhileEnd (/= '=')))
