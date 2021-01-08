{-# OPTIONS_HADDOCK hide, prune #-}

module Web.Pixiv.API.Illust
  ( module Web.Pixiv.API.Illust,
  )
where

import Servant.API
import Web.Pixiv.API.PixivEntry
import Web.Pixiv.Types

type IllustIdParam = QueryParam' '[Required, Strict] "illust_id" Int

type GetIllustDetail = PixivEntry :> "v1" :> "illust" :> "detail" :> IllustIdParam :> Get '[JSON] IllustWrapper

type GetIllustComments = PixivEntry :> "v1" :> "illust" :> "comments" :> IllustIdParam :> OffsetParam :> Get '[JSON] Comments

type GetIllustRelated = PixivEntry :> "v2" :> "illust" :> "related" :> IllustIdParam :> OffsetParam :> Get '[JSON] Illusts

type GetIllustRanking = PixivEntry :> "v1" :> "illust" :> "ranking" :> QueryParam "mode" RankMode :> OffsetParam :> Get '[JSON] Illusts

type GetIllustFollow = PixivEntry :> "v1" :> "illust" :> "follow" :> RestrictParam :> OffsetParam :> Get '[JSON] Illusts

type GetIllustNew = PixivEntry :> "v1" :> "illust" :> "new" :> OffsetParam :> Get '[JSON] Illusts

type GetUgoiraMetadata = PixivEntry :> "v1" :> "ugoira" :> "metadata" :> IllustIdParam :> Get '[JSON] UgoiraMetadataWrapper
