module Web.Pixiv.Types.Illust where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

type IllustIdParam = QueryParam' '[Required, Strict] "illust_id" Int

type GetIllustDetail = PixivEntry :> "v1" :> "illust" :> "detail" :> IllustIdParam :> Get '[JSON] IllustDetail

type GetIllustComments = PixivEntry :> "v1" :> "illust" :> "comments" :> IllustIdParam :> OffsetParam :> Get '[JSON] Comments

type GetIllustRelated = PixivEntry :> "v2" :> "illust" :> "related" :> IllustIdParam :> OffsetParam :> Get '[JSON] Illusts

type GetIllustRanking = PixivEntry :> "v1" :> "illust" :> "ranking" :> QueryParam "mode" RankMode :> OffsetParam :> Get '[JSON] Illusts

type GetIllustFollow = PixivEntry :> "v1" :> "illust" :> "follow" :> RestrictParam :> OffsetParam :> Get '[JSON] Illusts

type GetIllustNew = PixivEntry :> "v1" :> "illust" :> "new" :> OffsetParam :> Get '[JSON] Illusts
