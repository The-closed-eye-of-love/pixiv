module Web.Pixiv.Types.Trending where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

type GetTrendingTags =
  PixivEntry :> "v1" :> "trending-tags" :> "illust"
    :> QueryParam "include_translated_tag_results" Bool
    :> Get '[JSON] TrendingTags

type GetRecommendedIllusts =
  PixivEntry :> "v1" :> "illust" :> "recommended"
    :> QueryParam "include_privacy_policy" Bool
    :> QueryParam "include_ranking_illusts" Bool
    :> Get '[JSON] Illusts

type GetRecommendedMangas =
  PixivEntry :> "v1" :> "manga" :> "recommended"
    :> QueryParam "include_privacy_policy" Bool
    :> QueryParam "include_ranking_illusts" Bool
    :> Get '[JSON] Illusts
