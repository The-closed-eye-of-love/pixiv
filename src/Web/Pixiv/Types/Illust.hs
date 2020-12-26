module Web.Pixiv.Types.Illust where

import Deriving.Aeson.Stock
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

type IllustIdParam = QueryParam' '[Required, Strict] "illust_id" Int

newtype IllustDetail = IllustDetail
  { illust :: Illust
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake IllustDetail

type GetIllustDetail = PixivEntry :> "v1" :> "illust" :> "detail" :> IllustIdParam :> Get '[JSON] IllustDetail

type GetIllustComments = PixivEntry :> "v1" :> "illust" :> "comments" :> IllustIdParam :> OffsetParam :> Get '[JSON] Comments

type GetIllustRelated = PixivEntry :> "v2" :> "illust" :> "related" :> IllustIdParam :> OffsetParam :> Get '[JSON] Illusts
