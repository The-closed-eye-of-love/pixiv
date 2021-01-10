{-# OPTIONS_HADDOCK hide, prune #-}

module Web.Pixiv.API.Article
  ( module Web.Pixiv.API.Article,
  )
where

import Data.Text (Text)
import Servant.API
import Web.Pixiv.API.PixivEntry
import Web.Pixiv.Types

type GetSpotlightArticles = PixivEntry :> "v1" :> "spotlight" :> "articles" :> QueryParam "category" Text :> OffsetParam :> Get '[JSON] SpotlightArticles
