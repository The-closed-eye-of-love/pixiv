{-# LANGUAGE DuplicateRecordFields #-}

module Web.Pixiv.Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Deriving.Aeson
import Deriving.Aeson.Stock
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-----------------------------------------------------------------------------

type PixivJSON (k :: Symbol) = CustomJSON '[FieldLabelModifier (PixivLabelModifier k, CamelToSnake)]

type PixivJSON' = PixivJSON ""

-- | Strip prefix @'_'@ and @k@, making sure the result is non-empty
data PixivLabelModifier (k :: Symbol)

instance KnownSymbol k => StringModifier (PixivLabelModifier k) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @k)) s' of
    Nothing -> s'
    Just "" -> s'
    Just x -> x
    where
      s' = fromMaybe s (stripPrefix "_" s)

-----------------------------------------------------------------------------

type ImageUrl = Text

data ImageUrls = ImageUrls
  { _squareMedium :: ImageUrl,
    _medium :: ImageUrl,
    _large :: ImageUrl,
    _original :: ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' ImageUrls

newtype OriginalImageUrl = OriginalImageUrl
  { originalImageUrl :: ImageUrl
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

-----------------------------------------------------------------------------

data Series = Series
  { _seriesId :: Int,
    _title :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "series" Series

data IllustType = TypeIllust | TypeManga
  deriving stock (Eq, Show, Generic)

instance ToJSON IllustType where
  toJSON TypeIllust = A.String "illust"
  toJSON TypeManga = A.String "manga"

instance FromJSON IllustType where
  parseJSON = A.withText "IllustType" $ \case
    "illust" -> pure TypeIllust
    "manga" -> pure TypeManga
    _ -> mempty

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
    -- TODO
    _metaSinglePage :: A.Value,
    _metaPages :: [ImageUrls],
    _totalView :: Int,
    _totalBookmarks :: Int,
    _isBookmarked :: Bool,
    _visible :: Bool,
    _isMuted :: Bool,
    _totalComments :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON "illust" Illust

newtype Illusts = Illusts
  { _illusts :: [Illust]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Illusts

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
    _isPreminum :: Bool,
    _isUsingCustomProfileImage :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserProfile

data Publicity = Public | Private
  deriving stock (Eq, Show, Generic)

instance ToJSON Publicity where
  toJSON Public = A.String "public"
  toJSON Private = A.String "private"

instance FromJSON Publicity where
  parseJSON = A.withText "Publicity" $ \case
    "public" -> pure Public
    "private" -> pure Private
    _ -> mempty

data ProfilePublicity = ProfilePublicity
  { _gender :: Publicity,
    _region :: Publicity,
    _birthDay :: Publicity,
    _birthYear :: Publicity,
    _job :: Publicity,
    _pawoo :: Publicity
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

-----------------------------------------------------------------------------

data TrendingTag = TrendingTag
  { _tag :: Tag,
    _illust :: Illust
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON TrendingTag where
  parseJSON v =
    TrendingTag <$> A.parseJSON v <*> parse' v
    where
      parse' = A.withObject "TrendingTag" $ \o -> o A..: "_illust"

instance ToJSON TrendingTag where
  toJSON TrendingTag {..} = case A.toJSON _tag of
    (A.Object m) -> A.Object $ HashMap.insert "_illust" (A.toJSON _illust) m
    _ -> error "impossible"

newtype TrendingTags = TrendingTags
  { _trend_tags :: [TrendingTag]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' TrendingTags

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
    _comments :: [Comment]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' Comments

-----------------------------------------------------------------------------

data UserPreview = UserPreview
  { _user :: User,
    _illusts :: [Illust],
    -- TODO
    _novels :: A.Value,
    _isMuted :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserPreview

newtype UserPreviews = UserPreviews
  { _userPreviews :: [UserPreview]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PixivJSON' UserPreviews
