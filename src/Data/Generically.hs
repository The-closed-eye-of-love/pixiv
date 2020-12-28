{-# LANGUAGE UndecidableInstances #-}

module Data.Generically
  ( SnakeHTTP (..),
    PixivJSON,
    EnumJSON,
    PixivJSON',
    EnumJSON',
  )
where

import Data.Aeson
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text, pack)
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant.API

newtype SnakeHTTP a = SnakeHTTP {unSnakeHTTP :: a}

instance Generic a => Generic (SnakeHTTP a) where
  type Rep (SnakeHTTP a) = Rep a
  to = SnakeHTTP . to
  from = from . SnakeHTTP

instance (Generic a, GToHttpApiData (Rep a)) => ToHttpApiData (SnakeHTTP a) where
  toQueryParam a = gToQueryParam (from a)

class GToHttpApiData f where
  gToQueryParam :: f p -> Text

instance GToHttpApiData V1 where
  gToQueryParam = undefined

instance Constructor m => GToHttpApiData (C1 m U1) where
  gToQueryParam a = pack $ camelTo2 '_' $ conName a

instance (GToHttpApiData l, GToHttpApiData r) => GToHttpApiData (l :+: r) where
  gToQueryParam (L1 a) = gToQueryParam a
  gToQueryParam (R1 a) = gToQueryParam a

instance (GToHttpApiData a) => GToHttpApiData (D1 m a) where
  gToQueryParam (M1 a) = gToQueryParam a

-----------------------------------------------------------------------------

-- | Strip prefix @'_'@ and @k@ from fields' labels then convert to snake case, making sure results are non-empty
type PixivJSON (k :: Symbol) = CustomJSON '[FieldLabelModifier (PixivLabelModifier k, CamelToSnake)]

-- | Strip prefix @k@ from constructors' tags then convert to snake case
type EnumJSON (k :: Symbol) = CustomJSON '[ConstructorTagModifier (StripPrefix k, CamelToSnake)]

-- | Strip prefix @'_'@ from fields' labels then convert to snake case
type PixivJSON' = PixivJSON ""

-- | Convert constructors' tags to snake case
type EnumJSON' = EnumJSON ""

data PixivLabelModifier (k :: Symbol)

instance KnownSymbol k => StringModifier (PixivLabelModifier k) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @k)) s' of
    Nothing -> s'
    Just "" -> s'
    Just x -> x
    where
      s' = fromMaybe s (stripPrefix "_" s)
