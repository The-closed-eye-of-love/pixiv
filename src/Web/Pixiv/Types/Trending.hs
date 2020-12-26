module Web.Pixiv.Types.Trending where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

type GetTrendingTags = PixivEntry :> "v1" :> "trending-tags" :> "illust" :> Get '[JSON] TrendingTags
