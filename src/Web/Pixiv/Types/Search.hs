module Web.Pixiv.Types.Search where

import Data.Text (Text)
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

data SortingMethod
  = DateDescending
  | DateAscending
  deriving stock (Show, Eq)

instance ToHttpApiData SortingMethod where
  toQueryParam DateDescending = "date_desc"
  toQueryParam DateAscending = "date_asc"

data Duration
  = WithinLastDay
  | WithinLastMonth
  | WithinLastYear
  deriving stock (Show, Eq)

data SearchTarget = ExactMatchForTags | PartialMatchForTags | TitleAndCaption
  deriving stock (Show, Eq)

instance ToHttpApiData SearchTarget where
  toQueryParam ExactMatchForTags = "exact_match_for_tags"
  toQueryParam PartialMatchForTags = "partial_match_for_tags"
  toQueryParam TitleAndCaption = "title_and_caption"

instance ToHttpApiData Duration where
  toQueryParam WithinLastDay = "within_last_day"
  toQueryParam WithinLastMonth = "within_last_month"
  toQueryParam WithinLastYear = "within_last_year"

type SearchIllust =
  PixivEntry :> "v1" :> "search" :> "illust"
    :> QueryParam' '[Required, Strict] "search_target" SearchTarget
    :> QueryParam' '[Required, Strict] "word" Text
    :> QueryParam "include_translated_tag_results" Bool
    :> QueryParam "sort" SortingMethod
    :> QueryParam "duration" Duration
    :> OffsetParam
    :> Get '[JSON] Illusts
