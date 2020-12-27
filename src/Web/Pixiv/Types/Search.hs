module Web.Pixiv.Types.Search where

import Data.Generically
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

data SortingMethod
  = DateDesc
  | DateAsc
  deriving stock (Show, Eq, Generic)
  deriving (ToHttpApiData) via Generically SortingMethod

data Duration
  = WithinLastDay
  | WithinLastMonth
  | WithinLastYear
  deriving stock (Show, Eq, Generic)
  deriving (ToHttpApiData) via Generically Duration

data SearchTarget = ExactMatchForTags | PartialMatchForTags | TitleAndCaption
  deriving stock (Show, Eq, Generic)
  deriving (ToHttpApiData) via Generically SearchTarget

type SearchIllust =
  PixivEntry :> "v1" :> "search" :> "illust"
    :> QueryParam' '[Required, Strict] "search_target" SearchTarget
    :> QueryParam' '[Required, Strict] "word" Text
    :> QueryParam "include_translated_tag_results" Bool
    :> QueryParam "sort" SortingMethod
    :> QueryParam "duration" Duration
    :> OffsetParam
    :> Get '[JSON] Illusts
