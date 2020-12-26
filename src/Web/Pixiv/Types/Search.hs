module Web.Pixiv.Types.Search where

import Data.Text (Text)
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.Pixiv

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

instance ToHttpApiData Duration where
  toQueryParam WithinLastDay = "within_last_day"
  toQueryParam WithinLastMonth = "within_last_month"
  toQueryParam WithinLastYear = "within_last_year"

type SearchIllust =
  Pixiv :> "v1" :> "search" :> "illust"
    :> QueryParam' '[Required, Strict] "word" Text
    :> QueryParam' '[Required, Strict] "page" Int
    :> QueryParam "include_translated_tag_results" Bool
    :> QueryParam "sort" SortingMethod
    :> QueryParam "duration" Duration
    :> Get '[JSON] Illusts
