module Web.Pixiv.Types.Illust where

import Deriving.Aeson.Stock
import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.Pixiv

type IllustId = QueryParam' '[Required, Strict] "illust_id" Int

newtype IllustDetail = IllustDetail
  { illust :: Illust
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake IllustDetail

type GetIllustDetail = Pixiv :> "v1" :> "illust" :> "detail" :> IllustId :> Get '[JSON] IllustDetail

type GetIllustComments = Pixiv :> "v1" :> "illust" :> "comments" :> IllustId :> Get '[JSON] Comments

type GetIllustRelated = Pixiv :> "v2" :> "illust" :> "related" :> IllustId :> Get '[JSON] Illusts
