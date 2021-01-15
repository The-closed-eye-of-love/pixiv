{-# LANGUAGE DuplicateRecordFields #-}

-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Data types used in API. This module enables [DuplicateRecordFields](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DuplicateRecordFields),
-- so please consider using "Web.Pixiv.Types.Lens" to access fields smoothly.
module Web.Pixiv.Types
  ( -- * ImageUrl
    ImageUrl,
    ImageUrls (..),
    OriginalImageUrl (..),

    -- * Tag
    Tag (..),
    TrendingTag (..),
    TrendingTags (..),

    -- * Illustration
    Series (..),
    IllustType (..),
    MetaPage (..),
    Illust (..),
    Illusts (..),
    IllustWrapper (..),

    -- * User
    User (..),
    UserProfile (..),
    Publicity (..),
    ProfilePublicity (..),
    Workspace (..),
    UserDetail (..),
    UserPreview (..),
    UserPreviews (..),

    -- * Comment
    Comment (..),
    Comments (..),

    -- * NextUrl
    NextUrlLess,
    HasNextUrl (..),

    -- * Ugoria
    UgoiraFrame (..),
    ZipUrls (..),
    UgoiraMetadata (..),
    UgoiraMetadataWrapper (..),

    -- * Article
    SpotlightArticle (..),
    SpotlightArticles (..),

    -- * Http
    RankMode (..),
    SearchTarget (..),
    SortingMethod (..),
    Duration (..),
  )
where

import qualified Data.Aeson as A
import Data.Generically
import Data.Text (Text)
import Data.Time (UTCTime)

-- | Undecorate @next_url@ of a type.
--
-- @next_url@ is returned by some APIs for paging.
type family NextUrlLess a

-- | Class to get or unwrap @next_url@.
class HasNextUrl a where
  unNextUrl :: a -> NextUrlLess a
  getNextUrl :: a -> Maybe Text

-----------------------------------------------------------------------------

-- | Image urls are represented in 'Text'.
type ImageUrl = Text

-- | An object contains image urls.
--
-- * Example in 'Illust' or 'MetaPage':
--   @
--     "image_urls": {
--       "square_medium": "...",
--       "medium": "...",
--       "large": "..."
--     }
--   @
-- * Example in 'User':
--   @
--     "profile_image_urls": {
--       "medium": "..."
--     }
--   @
data ImageUrls = ImageUrls
  { _squareMedium :: Maybe ImageUrl,
    _medium :: Maybe ImageUrl,
    _large :: Maybe ImageUrl,
    _original :: Maybe ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' ImageUrls

-- | An object contains a single image url.
--
-- In 'Illust':
--   @
--     "meta_single_page": {
--       "original_image_url": "..."
--     }
--   @
newtype OriginalImageUrl = OriginalImageUrl
  { _originalImageUrl :: Maybe ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' OriginalImageUrl

-----------------------------------------------------------------------------

-- | A tag.
--
-- Example:
-- @
--   {
--     "name": "...",
--     "translated_name": null
--   }
-- @
data Tag = Tag
  { _name :: Text,
    _translatedName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Tag

-- | A trending tag.
--
-- Don't confuse with 'Tag'. 'TrendingTag' contains 'Illust',
-- and the textual name of the tag is called @tag@, instead of @name@ in 'Tag'.
data TrendingTag = TrendingTag
  { -- This is ugly, not consistent with normal 'Tag'
    _trendTag :: Text,
    _translatedName :: Maybe Text,
    _illust :: Illust
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "trend" TrendingTag

-- | A wrapper of 'TrendingTag's for JSON deserialization.
newtype TrendingTags = TrendingTags
  { _trend_tags :: [TrendingTag]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' TrendingTags

-----------------------------------------------------------------------------

-- | Manga series.
data Series = Series
  { _seriesId :: Int,
    _title :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "series" Series

-- | Type of illustration.
--
-- In pixiv API, all of illustrations, mangas, and ugoiras are represented in 'Illust' data type.
-- So they can be distinguished by 'IllustType'.
data IllustType = TypeIllust | TypeManga | TypeUgoira
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via EnumJSON "Type" IllustType
  deriving (ToHttpApiData) via PixivHttpApiData "Type" IllustType

-- | A page of 'Illust' containing 'ImageUrls'.
newtype MetaPage = MetaPage
  { _imageUrls :: ImageUrls
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' MetaPage

-- | Illustraion data type.
--
-- See 'IllustType'.
data Illust = Illust
  { _illustId :: Int,
    _title :: Text,
    _illustType :: IllustType,
    _imageUrls :: ImageUrls,
    _caption :: Text,
    _restrict :: Int,
    _user :: User,
    _tags :: [Tag],
    _tools :: [Text],
    _createDate :: UTCTime,
    _pageCount :: Int,
    _width :: Int,
    _height :: Int,
    _sanityLevel :: Int,
    _xRestrict :: Int,
    -- | Only manga may have this field.
    _series :: Maybe Series,
    -- | Only single page illustration has this field.
    _metaSinglePage :: Maybe OriginalImageUrl,
    -- | Only multi page illustration has this field.
    _metaPages :: [MetaPage],
    _totalView :: Int,
    _totalBookmarks :: Int,
    -- | For login account.
    _isBookmarked :: Bool,
    _visible :: Bool,
    _isMuted :: Bool,
    _totalComments :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "illust" Illust

-- | Response of API which returns illustrations.
data Illusts = Illusts
  { _illusts :: [Illust],
    _nextUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Illusts

type instance NextUrlLess Illusts = [Illust]

instance HasNextUrl Illusts where
  unNextUrl Illusts {..} = _illusts
  getNextUrl Illusts {..} = _nextUrl

-- | A wrapper of 'Illust' for JSON deserialization.
newtype IllustWrapper = IllustWrapper
  { _illust :: Illust
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' IllustWrapper

-----------------------------------------------------------------------------

-- | User data type.
data User = User
  { _userId :: Int,
    _name :: Text,
    _account :: Text,
    _profileImageUrls :: ImageUrls,
    _comment :: Maybe Text,
    -- | For login account.
    _isFollowed :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "user" User

-- | UserProfile data type.
--
-- Not sure if all fields are covered, and maybe some fields should be optional.
data UserProfile = UserProfile
  { _webpage :: Maybe Text,
    _gender :: Text,
    _birth :: Text,
    _birthDay :: Text,
    _birthYear :: Int,
    _region :: Text,
    _addressId :: Int,
    _countryCode :: Text,
    _job :: Text,
    _jobId :: Int,
    _totalFollowUsers :: Int,
    _totalMypixivUsers :: Int,
    _totalIllusts :: Int,
    _totalManga :: Int,
    _totalIllustBookmarksPublic :: Int,
    _totalIllustSeries :: Int,
    _totalNovelSeries :: Int,
    _backgroundImageUrl :: Maybe ImageUrl,
    _twitterAccount :: Text,
    _twitterUrl :: Maybe Text,
    _pawooUrl :: Maybe Text,
    _isPreminum :: Maybe Bool,
    _isUsingCustomProfileImage :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserProfile

-- | Publicity data type.
--
-- The value @public@ or @private@ are present in 'ProfilePublicity'.
-- This type is also used in @restrict@ query param.
data Publicity
  = Public
  | Private
  -- | May not be available in @restrict@ query param
  | Mypixiv
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via EnumJSON' Publicity
  deriving (ToHttpApiData) via PixivHttpApiData' Publicity

-- | Profile publicity of a user.
--
-- Not sure if all fields are covered, and maybe some fields should be optional.
data ProfilePublicity = ProfilePublicity
  { _gender :: Publicity,
    _region :: Publicity,
    _birthDay :: Publicity,
    _birthYear :: Publicity,
    _job :: Publicity,
    _pawoo :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' ProfilePublicity

-- | Workspace information of a user.
-- Not sure if all fields are covered, and maybe some fields should be optional.
data Workspace = Workspace
  { _pc :: Text,
    _monitor :: Text,
    _tool :: Text,
    _scanner :: Text,
    _tablet :: Text,
    _mouse :: Text,
    _printer :: Text,
    _desktop :: Text,
    _music :: Text,
    _desk :: Text,
    _chair :: Text,
    _comment :: Text,
    _workspaceImageUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Workspace

-- | Details of a user.
data UserDetail = UserDetail
  { _user :: User,
    _profile :: UserProfile,
    _profilePublicity :: ProfilePublicity,
    _workspace :: Workspace
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserDetail

-- | A preview of user information.
--
-- Except 'Web.Pixiv.API.getUserDetail', other API involving users return this data type.
data UserPreview = UserPreview
  { _user :: User,
    _illusts :: [Illust],
    -- | Novels are not supported currently.
    _novels :: A.Value,
    _isMuted :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserPreview

-- | Response of API which returns user previews.
data UserPreviews = UserPreviews
  { _userPreviews :: [UserPreview],
    _nextUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserPreviews

type instance NextUrlLess UserPreviews = [UserPreview]

instance HasNextUrl UserPreviews where
  unNextUrl UserPreviews {..} = _userPreviews
  getNextUrl UserPreviews {..} = _nextUrl

-----------------------------------------------------------------------------

-- | A comment.
data Comment = Comment
  { _commentId :: Int,
    _comment :: Text,
    _date :: UTCTime,
    _user :: User,
    -- TODO
    _parentComment :: Maybe A.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "comment" Comment

-- | Response of API which returns comments.
data Comments = Comments
  { _totalComments :: Int,
    _comments :: [Comment],
    _nextUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Comments

type instance NextUrlLess Comments = [Comment]

instance HasNextUrl Comments where
  unNextUrl Comments {..} = _comments
  getNextUrl Comments {..} = _nextUrl

-----------------------------------------------------------------------------

-- | A ugoira frame.
data UgoiraFrame = UgoiraFrame
  { -- | File name.
    _ugoiraFile :: Text,
    -- | Duration of this frame (in millisecond).
    _ugoiraDelay :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "ugoira" UgoiraFrame

-- | A wrapper of ugoira zip file url.
newtype ZipUrls = ZipUrls
  { _zipMedium :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "zip" ZipUrls

-- | Ugoira is a frame animation, whose common information is represented in 'Illust'.
-- This metadata contains a link to download the zip archive, which compresses frames of the ugoira;
-- and 'UgoiraFrame's to represents metadata of each frame.
-- Using 'Web.Pixiv.API.getUgoiraMetadata' can obtain value of this type.
-- See 'Web.Pixiv.Utils.ugoiraMetadataToFFConcat' and 'Web.Pixiv.Download.downloadUgoiraToMP4'.
data UgoiraMetadata = UgoiraMetadata
  { _zipUrls :: ZipUrls,
    _frames :: [UgoiraFrame]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UgoiraMetadata

-- | A wrapper of 'UgoiraMetadata' for JSON deserialization.
newtype UgoiraMetadataWrapper = UgoiraMetadataWrapper
  { _ugoiraMetadata :: UgoiraMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UgoiraMetadataWrapper

-----------------------------------------------------------------------------

-- | Spotlight article.
data SpotlightArticle = SpotlightArticle
  { _saId :: Int,
    _title :: Text,
    _pureTitle :: Text,
    _thumbnail :: Text,
    _articleUrl :: Text,
    _publishDate :: UTCTime,
    _category :: Text,
    _subcategoryLabel :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' SpotlightArticle

-- | Response of API which returns spotlight articles.
data SpotlightArticles = SpotlightArticles
  { _spotlightArticles :: [SpotlightArticle],
    _nextUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' SpotlightArticles

type instance NextUrlLess SpotlightArticles = [SpotlightArticle]

instance HasNextUrl SpotlightArticles where
  unNextUrl SpotlightArticles {..} = _spotlightArticles
  getNextUrl SpotlightArticles {..} = _nextUrl

-----------------------------------------------------------------------------

-- | Rank mode query parm.
--
-- See 'Web.Pixiv.API.getIllustRanking'
data RankMode
  = Day
  | DayR18
  | DayMale
  | DayMaleR18
  | DayFemale
  | DayFemaleR18
  | Week
  | WeekR18
  | WeekR18G
  | Month
  | WeekOriginal
  | WeekRookie
  | DayManga
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via PixivHttpApiData' RankMode

-- | Sorting method query parm.
--
-- See 'Web.Pixiv.API.searchIllust' and 'Web.Pixiv.API.searchUser'.
data SortingMethod
  = DateDesc
  | DateAsc
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via PixivHttpApiData' SortingMethod

-- | Duration query parm.
--
-- See 'Web.Pixiv.API.searchIllust'.
data Duration
  = WithinLastDay
  | WithinLastMonth
  | WithinLastYear
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToHttpApiData) via PixivHttpApiData' Duration

-- | Search target query parm.
--
-- See 'Web.Pixiv.API.searchIllust'.
data SearchTarget = ExactMatchForTags | PartialMatchForTags | TitleAndCaption
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via PixivHttpApiData' SearchTarget
