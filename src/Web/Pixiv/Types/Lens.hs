{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Lenses of "Web.Pixiv.Types".
module Web.Pixiv.Types.Lens
  ( module Web.Pixiv.Types.Lens,
  )
where

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
makeFieldsNoPrefix ''MetaPage

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''User
makeFieldsNoPrefix ''UserProfile
makeFieldsNoPrefix ''ProfilePublicity
makeFieldsNoPrefix ''Workspace
makeFieldsNoPrefix ''UserDetail

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''TrendingTag

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''Comment

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''UserPreview

-----------------------------------------------------------------------------
makeFieldsNoPrefix ''UgoiraFrame
makeFieldsNoPrefix ''ZipUrls
makeFieldsNoPrefix ''UgoiraMetadata
