{-# LANGUAGE DuplicateRecordFields #-}

module Web.Pixiv.Types
  ( -- * Image
    ImageUrls (..),
    OriginalImageUrl (..),

    -- * Tag
    Tag (..),
    TrendingTag (..),
    TrendingTags (..),

    -- * Illust
    Series (..),
    IllustType (..),
    MetaPage (..),
    Illust (..),
    Illusts (..),
    IllustDetail (..),

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

    -- * Http
    RankMode (..),
    SearchTarget (..),
    SortingMethod (..),
    Duration (..),
  )
where

import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Time (UTCTime)
import Deriving.Aeson
import Servant.API (ToHttpApiData (..))
import Data.Generically

type family NextUrlLess a

class HasNextUrl a where
  unNextUrl :: a -> NextUrlLess a
  getNextUrl :: a -> Maybe Text

-----------------------------------------------------------------------------

type ImageUrl = Text

data ImageUrls = ImageUrls
  { _squareMedium :: Maybe ImageUrl,
    _medium :: Maybe ImageUrl,
    _large :: Maybe ImageUrl,
    _original :: Maybe ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' ImageUrls

newtype OriginalImageUrl = OriginalImageUrl
  { _originalImageUrl :: Maybe ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' OriginalImageUrl

-----------------------------------------------------------------------------

data Tag = Tag
  { _name :: Text,
    _translatedName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Tag

data TrendingTag = TrendingTag
  { -- This is ugly, not consistent with normal 'Tag'
    _trendTag :: Text,
    _translatedName :: Maybe Text,
    _illust :: Illust
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "trend" TrendingTag

newtype TrendingTags = TrendingTags
  { _trend_tags :: [TrendingTag]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' TrendingTags

-----------------------------------------------------------------------------

data Series = Series
  { _seriesId :: Int,
    _title :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "series" Series

data IllustType = TypeIllust | TypeManga
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via EnumJSON "Type" IllustType

instance ToHttpApiData IllustType where
  toQueryParam TypeIllust = "illust"
  toQueryParam TypeManga = "manga"

newtype MetaPage = MetaPage
  { _imageUrls :: ImageUrls
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' MetaPage

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
    _series :: Maybe Series,
    _metaSinglePage :: Maybe OriginalImageUrl,
    _metaPages :: [MetaPage],
    _totalView :: Int,
    _totalBookmarks :: Int,
    _isBookmarked :: Bool,
    _visible :: Bool,
    _isMuted :: Bool,
    _totalComments :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "illust" Illust

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

newtype IllustDetail = IllustDetail
  { _illust :: Illust
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' IllustDetail

-----------------------------------------------------------------------------
data User = User
  { _userId :: Int,
    _name :: Text,
    _account :: Text,
    _profileImageUrls :: ImageUrls,
    _comment :: Maybe Text,
    _isFollowed :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "user" User

data UserProfile = UserProfile
  { _webpage :: Text,
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
    _totalManga :: Int,
    _totalIllustBookmarksPublic :: Int,
    _totalIllustSeries :: Int,
    _totalNovelSeries :: Int,
    _backgroundImageUrl :: ImageUrl,
    _twitterAccount :: Text,
    _twitterUrl :: Text,
    _pawooUrl :: Maybe Text,
    _isPreminum :: Maybe Bool,
    _isUsingCustomProfileImage :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserProfile

data Publicity = Public | Private
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via EnumJSON' Publicity
  deriving (ToHttpApiData) via SnakeHTTP Publicity

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

data UserDetail = UserDetail
  { _user :: User,
    _profile :: UserProfile,
    _profilePublicity :: ProfilePublicity,
    _workspace :: Workspace
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserDetail

data UserPreview = UserPreview
  { _user :: User,
    _illusts :: [Illust],
    -- TODO
    _novels :: A.Value,
    _isMuted :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserPreview

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

data RankMode
  = Day
  | Week
  | Month
  | DayMale
  | DayFemale
  | WeekOriginal
  | WeekRookie
  | DayR18
  | DayManga
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via SnakeHTTP RankMode

data SortingMethod
  = DateDesc
  | DateAsc
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via SnakeHTTP SortingMethod

data Duration
  = WithinLastDay
  | WithinLastMonth
  | WithinLastYear
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToHttpApiData) via SnakeHTTP Duration

data SearchTarget = ExactMatchForTags | PartialMatchForTags | TitleAndCaption
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (ToHttpApiData) via SnakeHTTP SearchTarget
