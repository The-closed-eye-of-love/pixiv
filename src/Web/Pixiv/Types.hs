{-# LANGUAGE DuplicateRecordFields #-}

module Web.Pixiv.Types where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Deriving.Aeson
import Deriving.Aeson.Stock

type ImageUrl = Text

data ImageUrls = ImageUrls
  { squareMedium :: ImageUrl,
    medium :: ImageUrl,
    large :: ImageUrl,
    original :: ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake ImageUrls

newtype OriginalImageUrl = OriginalImageUrl
  { originalImageUrl :: ImageUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake OriginalImageUrl

-----------------------------------------------------------------------------
type TagName = Text

data Tag = Tag
  { name :: TagName,
    translatedName :: Maybe TagName
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake Tag

type Tags = [Tag]

-----------------------------------------------------------------------------

type Date = Text

data Series = Series
  { seriesId :: Int,
    title :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier (StripPrefix "series", CamelToSnake)] Series

data IllustType = TypeIllust | TypeManga
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier (StripPrefix "Type", CamelToSnake)] IllustType

data Illust = Illust
  { illustId :: Int,
    title :: Text,
    illustType :: IllustType,
    imageUrls :: ImageUrls,
    caption :: Text,
    restrict :: Int,
    user :: User,
    tags :: Tags,
    tools :: [Text],
    createDate :: Date,
    pageCount :: Int,
    width :: Int,
    height :: Int,
    sanityLevel :: Int,
    xRestrict :: Int,
    series :: Maybe Series,
    -- TODO
    metaSinglePage :: Value,
    metaPages :: [ImageUrls],
    totalView :: Int,
    totalBookmarks :: Int,
    isBookmarked :: Bool,
    visible :: Bool,
    isMuted :: Bool,
    totalComments :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier (StripPrefix "illust", CamelToSnake)] Illust

-----------------------------------------------------------------------------
data User = User
  { userId :: Int,
    name :: Text,
    account :: Text,
    profileImageUrls :: ImageUrls,
    comment :: Maybe Text,
    isFollowed :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier (StripPrefix "user", CamelToSnake)] User

data UserProfile = UserProfile
  { webpage :: Text,
    gender :: Text,
    birth :: Text,
    birthDay :: Text,
    birthYear :: Int,
    region :: Text,
    addressId :: Int,
    countryCode :: Text,
    job :: Text,
    jobId :: Int,
    totalFollowUsers :: Int,
    totalMypixivUsers :: Int,
    totalManga :: Int,
    totalIllustBookmarksPublic :: Int,
    totalIllustSeries :: Int,
    totalNovelSeries :: Int,
    backgroundImageUrl :: ImageUrl,
    twitterAccount :: Text,
    twitterUrl :: Text,
    pawooUrl :: Maybe Text,
    isPreminum :: Bool,
    isUsingCustomProfileImage :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake UserProfile

data Publicity = Public | Private
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake Publicity

data ProfilePublicity = ProfilePublicity
  { gender :: Publicity,
    region :: Publicity,
    birthDay :: Publicity,
    birthYear :: Publicity,
    job :: Publicity,
    pawoo :: Publicity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Workspace = Workspace
  { pc :: Text,
    monitor :: Text,
    tool :: Text,
    scanner :: Text,
    tablet :: Text,
    mouse :: Text,
    printer :: Text,
    desktop :: Text,
    music :: Text,
    desk :: Text,
    chair :: Text,
    comment :: Text,
    workspaceImageUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Snake Workspace