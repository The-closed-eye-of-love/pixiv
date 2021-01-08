{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- This module provides some generic functions to create instances
-- of 'FromJSON', 'Data.Aeson.toJSON', and 'ToHttpApiData' using [DerivingVia](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia).
-- You can find usages in "Web.Pixiv.Types".
module Data.Generically
  ( -- * HttpApiData
    CustomHttpApiData (..),
    CustomHttpApiDataOption (..),
    GToHttpApiData (..),
    PixivHttpApiData,
    PixivHttpApiData',

    -- * JSON
    PixivJSON,
    EnumJSON,
    PixivJSON',
    EnumJSON',
    PixivLabelModifier,

    -- * Re-export
    ToHttpApiData (..),
    module Deriving.Aeson,
  )
where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant.API (ToHttpApiData (..))

-----------------------------------------------------------------------------

-- | A wrapper to derive 'ToHttpApiData' instances with modifications.
newtype CustomHttpApiData t a = CustomHttpApiData {unCustomHttpApiData :: a}

-- | Strips prefix @k@ from query parameter's name then converts it to snake case.
type PixivHttpApiData (k :: Symbol) = CustomHttpApiData '[StripPrefix k, CamelToSnake]

-- | Converts query parameter's name to snake case.
type PixivHttpApiData' = PixivHttpApiData ""

-- | Reifies 'StringModifier's applied on 'CustomHttpApiData'.
class CustomHttpApiDataOption xs where
  option :: String -> String

instance CustomHttpApiDataOption '[] where
  option = id

instance (StringModifier f, CustomHttpApiDataOption xs) => CustomHttpApiDataOption (f ': xs) where
  option = (option @xs) . getStringModifier @f

instance Generic a => Generic (CustomHttpApiData t a) where
  type Rep (CustomHttpApiData t a) = Rep a
  to = CustomHttpApiData . to
  from = from . unCustomHttpApiData

instance (Generic a, CustomHttpApiDataOption t, GToHttpApiData (Rep a)) => ToHttpApiData (CustomHttpApiData t a) where
  toQueryParam a = gToQueryParam (from a) (option @t)

-- | Class of generic representation types that can be converted to query param.
class GToHttpApiData f where
  gToQueryParam :: f p -> (String -> String) -> Text

instance GToHttpApiData V1 where
  gToQueryParam a = case a of

instance Constructor m => GToHttpApiData (C1 m U1) where
  gToQueryParam a f = pack $ f $ conName a

instance (GToHttpApiData l, GToHttpApiData r) => GToHttpApiData (l :+: r) where
  gToQueryParam (L1 a) = gToQueryParam a
  gToQueryParam (R1 a) = gToQueryParam a

instance (GToHttpApiData a) => GToHttpApiData (D1 m a) where
  gToQueryParam (M1 a) = gToQueryParam a

-----------------------------------------------------------------------------

-- | Strips prefix @_@ and @k@ from /fields' labels/ then converts it to snake case, making sure results are non-empty.
type PixivJSON (k :: Symbol) = CustomJSON '[FieldLabelModifier (PixivLabelModifier k, CamelToSnake)]

-- | Strips prefix @k@ from /constructors' tags/ then converts it to snake case.
type EnumJSON (k :: Symbol) = CustomJSON '[ConstructorTagModifier (StripPrefix k, CamelToSnake)]

-- | Strips prefix @_@ from /fields' labels/ then converts it to snake case.
type PixivJSON' = PixivJSON ""

-- | Converts /constructors' tags/ to snake case.
type EnumJSON' = EnumJSON ""

-- | A 'StringModifier' that strips prefix @_@ and @k@, and returns non-empty 'String'.
data PixivLabelModifier (k :: Symbol)

instance KnownSymbol k => StringModifier (PixivLabelModifier k) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @k)) s' of
    Nothing -> s'
    Just "" -> s'
    Just x -> x
    where
      s' = fromMaybe s (stripPrefix "_" s)
