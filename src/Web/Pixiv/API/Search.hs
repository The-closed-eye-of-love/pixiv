{-# OPTIONS_HADDOCK hide, prune #-}

module Web.Pixiv.API.Search
  ( module Web.Pixiv.API.Search,
  )
where

import Data.Text (Text)
import Servant.API
import Web.Pixiv.API.PixivEntry
import Web.Pixiv.Types

type SearchIllust =
  PixivEntry :> "v1" :> "search" :> "illust"
    :> QueryParam' '[Required, Strict] "search_target" SearchTarget
    :> QueryParam' '[Required, Strict] "word" Text
    :> QueryParam "include_translated_tag_results" Bool
    :> QueryParam "sort" SortingMethod
    :> QueryParam "duration" Duration
    :> OffsetParam
    :> Get '[JSON] Illusts

type SearchUser =
  PixivEntry :> "v1" :> "search" :> "user"
    :> QueryParam' '[Required, Strict] "word" Text
    :> OffsetParam
    :> Get '[JSON] UserPreviews
