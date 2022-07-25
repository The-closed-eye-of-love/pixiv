{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- This module provides some TH functions to create instances
-- of 'FromJSON', 'ToJSON', and 'ToHttpApiData'.
-- You can find usages in "Web.Pixiv.Types" and "Web.Pixiv.Auth".
module Web.Pixiv.TH
  ( derivePixivJSON,
    derivePixivJSON',
    deriveEnumJSON,
    deriveEnumJSON',
    deriveEnumToHttpApiData,
    deriveEnumToHttpApiData',
    ToHttpApiData (..),
    FromJSON (..),
    ToJSON (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), camelTo2)
import Data.Aeson.TH
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Haskell.TH
import Servant.API (ToHttpApiData (..))

-----------------------------------------------------------------------------

-- | Creates instances of 'FromJSON' and 'ToJSON',
-- stripping @_@ and @prefix@ from field labels (making sure result is non-empty),
-- then converting into snake case.
derivePixivJSON :: String -> Name -> DecsQ
derivePixivJSON prefix =
  deriveJSON
    defaultOptions
      { fieldLabelModifier = modifyFieldName prefix
      }

-- | Like 'derivePixivJSON' but does not strip prefix.
derivePixivJSON' :: Name -> DecsQ
derivePixivJSON' = derivePixivJSON ""

-----------------------------------------------------------------------------

-- | Creates instances of 'FromJSON' and 'ToJSON',
-- stripping @prefix@ from constructor tags then converting
-- into snake case.
deriveEnumJSON :: String -> Name -> DecsQ
deriveEnumJSON prefix =
  deriveJSON
    defaultOptions
      { constructorTagModifier = modifyConsturctorName prefix
      }

-- | Like 'deriveEnumJSON' but does not strip prefix.
deriveEnumJSON' :: Name -> DecsQ
deriveEnumJSON' = deriveEnumJSON ""

-----------------------------------------------------------------------------

-- | Creates instance of 'ToHttpApiData' for a enum-like data type
-- which contains only plain normal constructors.
--
-- Constructor tags will be stripped @prefix@, then converted into snake case.
deriveEnumToHttpApiData :: String -> Name -> DecsQ
deriveEnumToHttpApiData prefix name =
  reify name >>= \case
    TyConI dec -> case dec of
      (DataD _ _dataName [] _ cons _) -> do
        let conNames = [n | (NormalC n []) <- cons]
        if length cons /= length conNames
          then error "Data type is not simply an enum"
          else do
            let clauses =
                  [ clause
                      [conP n []]
                      (normalB [|T.pack $ modifyConsturctorName prefix n'|])
                      []
                    | n <- conNames,
                      let n' = nameBase n
                  ]
            func <- funD (mkName "toQueryParam") clauses
            pure [InstanceD Nothing [] (ConT ''ToHttpApiData `AppT` ConT name) [func]]
      _ -> error "Unsupported data declaration"
    _ -> error "Not a plain type constructor"

-- | Like 'deriveEnumToHttpApiData' but does not strip prefix.
deriveEnumToHttpApiData' :: Name -> DecsQ
deriveEnumToHttpApiData' = deriveEnumToHttpApiData ""

-----------------------------------------------------------------------------

camel2Snake :: String -> String
camel2Snake = camelTo2 '_'

modifyFieldName :: String -> String -> String
modifyFieldName prefix s = camel2Snake $ case stripPrefix prefix s' of
  Nothing -> s'
  Just "" -> s'
  Just x -> x
  where
    s' = fromMaybe s (stripPrefix "_" s)

modifyConsturctorName :: String -> String -> String
modifyConsturctorName prefix = camel2Snake . (fromMaybe <*> stripPrefix prefix)
