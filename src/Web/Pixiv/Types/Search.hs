module Web.Pixiv.Types.Search where

import Data.Text (Text)
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

type SearchIllust =
  PixivEntry :> "v1" :> "search" :> "illust"
    :> QueryParam' '[Required, Strict] "search_target" SearchTarget
    :> QueryParam' '[Required, Strict] "word" Text
    :> QueryParam "include_translated_tag_results" Bool
    :> QueryParam "sort" SortingMethod
    :> QueryParam "duration" Duration
    :> OffsetParam
    :> Get '[JSON] Illusts
