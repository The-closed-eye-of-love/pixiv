{-# LANGUAGE UndecidableInstances #-}

module Data.Generically where

import Data.Aeson
import Data.Text (Text, pack)
import GHC.Generics
import Servant.API

newtype Generically a = Generically {unGenerically :: a}

instance Generic a => Generic (Generically a) where
  type Rep (Generically a) = Rep a
  to = Generically . to
  from = from . unGenerically

instance (Generic a, GToHttpApiData (Rep a)) => ToHttpApiData (Generically a) where
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
