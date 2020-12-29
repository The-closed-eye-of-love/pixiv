{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generically
  ( CustomHttpApiData (..),
    PixivHttpApiData,
    PixivHttpApiData',
    PixivJSON,
    EnumJSON,
    PixivJSON',
    EnumJSON',
  )
where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text, pack)
import Deriving.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant.API

newtype CustomHttpApiData t a = CustomHttpApiData {unCustomHttpApiData :: a}

type PixivHttpApiData (k :: Symbol) = CustomHttpApiData '[StripPrefix k, CamelToSnake]

type PixivHttpApiData' = CustomHttpApiData '[CamelToSnake]

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

class GToHttpApiData f where
  gToQueryParam :: f p -> (String -> String) -> Text

instance GToHttpApiData V1 where
  gToQueryParam = undefined

instance Constructor m => GToHttpApiData (C1 m U1) where
  gToQueryParam a f = pack $ f $ conName a

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
