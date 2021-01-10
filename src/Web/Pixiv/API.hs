-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Functions to access pixiv api. They are supposed to be run in "Web.Pixiv.Types.PixivT".
--
-- You may notice that except 'getUserBookmarks', many of functions take a @page@ as input.
-- This is because each query contains 30 entries (except 'getSpotlightArticles''s , which containes 10), i.e. you should pass @2@ if you want items ranged from 31 to 60.
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

    -- * Article
    getSpotlightArticles,
  )
where

import Control.Lens
import Data.Coerce (coerce)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Servant.Client.Core
import Web.Pixiv.API.Article
import Web.Pixiv.API.Illust
import Web.Pixiv.API.PixivEntry (pageToOffset)
import Web.Pixiv.API.Search
import Web.Pixiv.API.Trending
import Web.Pixiv.API.User
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivT

-----------------------------------------------------------------------------

-- | Gets trending tags.
getTrendingTags ::
  MonadPixiv m =>
  -- | includes translated tag results
  Maybe Bool ->
  m [TrendingTag]
getTrendingTags includeTranslatedTagResults = do
  p <- getAccessTokenWithAccpetLanguage
  tags <- clientIn (Proxy @GetTrendingTags) Proxy p includeTranslatedTagResults
  pure $ coerce tags

-- | Gets recommended illustrations of the account which is used for login.
getRecommendedIllusts ::
  MonadPixiv m =>
  -- | includes privacy policy
  Maybe Bool ->
  -- | includes translated tag results
  Maybe Bool ->
  m [Illust]
getRecommendedIllusts includePrivacyPolicy includeTranslatedTagResults = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetRecommendedIllusts) Proxy p includePrivacyPolicy includeTranslatedTagResults
  pure $ unNextUrl illusts

-- | Gets recommended mangas of the account which is used for login.
getRecommendedMangas :: MonadPixiv m => Maybe Bool -> Maybe Bool -> m [Illust]
getRecommendedMangas includePrivacyPolicy includeTranslatedTagResults = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetRecommendedMangas) Proxy p includePrivacyPolicy includeTranslatedTagResults
  pure $ unNextUrl illusts

-----------------------------------------------------------------------------

-- | Gets the details of a illustration.
getIllustDetail ::
  MonadPixiv m =>
  -- | illust id
  Int ->
  m Illust
getIllustDetail illustId = do
  p <- getAccessTokenWithAccpetLanguage
  detail <- clientIn (Proxy @GetIllustDetail) Proxy p illustId
  pure $ coerce detail

-- | Gets the comments of a illustration.
getIllustComments ::
  MonadPixiv m =>
  -- | illust id
  Int ->
  -- | page
  Int ->
  m Comments
getIllustComments illustId (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  clientIn (Proxy @GetIllustComments) Proxy p illustId offset

-- | Gets related illustrations.
getIllustRelated ::
  MonadPixiv m =>
  -- | illust id
  Int ->
  -- | page
  Int ->
  m [Illust]
getIllustRelated illustId (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustRelated) Proxy p illustId offset
  pure $ unNextUrl illusts

-- | Gets ranking illustrations.
getIllustRanking ::
  MonadPixiv m =>
  -- | rank mode
  Maybe RankMode ->
  -- | page
  Int ->
  m [Illust]
getIllustRanking mode (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustRanking) Proxy p mode offset
  pure $ unNextUrl illusts

-- | Gets illustrations of artists which the login account follows.
getIllustFollow ::
  MonadPixiv m =>
  -- | restrict
  Publicity ->
  -- | page
  Int ->
  m [Illust]
getIllustFollow restrict (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustFollow) Proxy p restrict offset
  pure $ unNextUrl illusts

-- | Gets the newest illustrations.
getIllustNew ::
  MonadPixiv m =>
  -- | page
  Int ->
  m [Illust]
getIllustNew (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetIllustNew) Proxy p offset
  pure $ unNextUrl illusts

-- | Gets the metadata of a ugoira
getUgoiraMetadata ::
  MonadPixiv m =>
  -- | illust id
  Int ->
  m UgoiraMetadata
getUgoiraMetadata illustId = do
  p <- getAccessTokenWithAccpetLanguage
  ug <- clientIn (Proxy @GetUgoiraMetadata) Proxy p illustId
  pure $ coerce ug

-----------------------------------------------------------------------------

-- | Searches an illustration.
searchIllust ::
  MonadPixiv m =>
  -- | search target
  SearchTarget ->
  -- | word
  Text ->
  -- | includes translated tag
  Maybe Bool ->
  -- | sorting method
  Maybe SortingMethod ->
  -- | duration
  Maybe Duration ->
  -- | page
  Int ->
  m [Illust]
searchIllust searchTarget searchWord includeTranslatedTag sortingMethod duration (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @SearchIllust) Proxy p searchTarget searchWord includeTranslatedTag sortingMethod duration offset
  pure $ unNextUrl illusts

-- | Searches an user.
searchUser ::
  MonadPixiv m =>
  -- | word
  Text ->
  -- | sorting method
  Maybe SortingMethod ->
  -- | page
  Int ->
  m [UserPreview]
searchUser searchWord sortingMethod (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @SearchUser) Proxy p searchWord sortingMethod offset
  pure $ unNextUrl ups

-----------------------------------------------------------------------------

-- | Gets the details of an user.
getUserDetail ::
  MonadPixiv m =>
  -- | user id
  Int ->
  m UserDetail
getUserDetail userId = do
  p <- getAccessTokenWithAccpetLanguage
  clientIn (Proxy @GetUserDetail) Proxy p userId

-- | Gets illustrations submitted by a specific user.
getUserIllusts ::
  MonadPixiv m =>
  -- | user id
  Int ->
  -- | illust type
  Maybe IllustType ->
  -- | page
  Int ->
  m [Illust]
getUserIllusts userId illustType (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetUserIllusts) Proxy p userId illustType offset
  pure $ unNextUrl illusts

-- |  Gets the following of an user.
getUserFollowing ::
  MonadPixiv m =>
  -- | user id
  Int ->
  -- | restrict
  Publicity ->
  -- | page
  Int ->
  m [UserPreview]
getUserFollowing userId restrict (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserFollowing) Proxy p userId restrict offset
  pure $ unNextUrl ups

-- | Gets the followers of an user.
getUserFollower ::
  MonadPixiv m =>
  -- | user id
  Int ->
  -- | page
  Int ->
  m [UserPreview]
getUserFollower userId (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserFollower) Proxy p userId offset
  pure $ unNextUrl ups

-- | Gets @mypixiv@ of an user.
getUserMypixiv ::
  MonadPixiv m =>
  -- | user id
  Int ->
  -- | page
  Int ->
  m [UserPreview]
getUserMypixiv userId (pageToOffset 30 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  ups <- clientIn (Proxy @GetUserMypixiv) Proxy p userId offset
  pure $ unNextUrl ups

-- | Gets illustrations collected by a specific user.
-- Returns the a list of illustrations and result and an integer for paging.
getUserBookmarks ::
  MonadPixiv m =>
  -- | user id
  Int ->
  -- | restrict
  Publicity ->
  -- | the last illust id of this query
  Maybe Int ->
  m ([Illust], Maybe Int)
getUserBookmarks userId restrict maxBookmarkId = do
  p <- getAccessTokenWithAccpetLanguage
  illusts <- clientIn (Proxy @GetUserBookmarks) Proxy p userId restrict maxBookmarkId
  pure (unNextUrl illusts, getNextUrl illusts >>= ((^? _Right . _1) . decimal . T.takeWhileEnd (/= '=')))

-----------------------------------------------------------------------------

-- | Gets spotlight articles
getSpotlightArticles ::
  MonadPixiv m =>
  -- | category
  Maybe Text ->
  -- | page
  Int ->
  m [SpotlightArticle]
getSpotlightArticles category (pageToOffset 10 -> offset) = do
  p <- getAccessTokenWithAccpetLanguage
  articles <- clientIn (Proxy @GetSpotlightArticles) Proxy p category offset
  pure $ unNextUrl articles
