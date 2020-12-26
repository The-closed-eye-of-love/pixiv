module Web.Pixiv.Types.Trending where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.Pixiv

type GetTrendingTags = Pixiv :> "v1" :> "trending-tags" :> "illust" :> Get '[JSON] TrendingTags
