{-# LANGUAGE TemplateHaskell #-}

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

derivePixivJSON :: String -> Name -> DecsQ
derivePixivJSON prefix =
  deriveJSON
    defaultOptions
      { fieldLabelModifier = modifyFieldName prefix
      }

derivePixivJSON' :: Name -> DecsQ
derivePixivJSON' = derivePixivJSON ""

-----------------------------------------------------------------------------

deriveEnumJSON :: String -> Name -> DecsQ
deriveEnumJSON prefix =
  deriveJSON
    defaultOptions
      { constructorTagModifier = modifyConsturctorName prefix
      }

deriveEnumJSON' :: Name -> DecsQ
deriveEnumJSON' = deriveEnumJSON ""

-----------------------------------------------------------------------------

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
