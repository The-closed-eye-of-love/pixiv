{-# LANGUAGE TemplateHaskell #-}

module Web.Pixiv.Types.Lens where

import Control.Lens.TH
import Web.Pixiv.Types

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''ImageUrls
makeFieldsNoPrefix ''OriginalImageUrl

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''Tag

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''Series
makeFieldsNoPrefix ''Illust
makeFieldsNoPrefix ''Illusts

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''User
makeFieldsNoPrefix ''UserProfile
makeFieldsNoPrefix ''ProfilePublicity
makeFieldsNoPrefix ''Workspace
makeFieldsNoPrefix ''UserDetail

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''TrendingTag
makeFieldsNoPrefix ''TrendingTags

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''Comment
makeFieldsNoPrefix ''Comments

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''UserPreview
makeFieldsNoPrefix ''UserPreviews