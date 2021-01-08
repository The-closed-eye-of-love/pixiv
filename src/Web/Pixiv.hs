-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- Example usage:
--
-- @
-- import Web.Pixiv
--
-- main :: IO ()
-- main = do
--   let credential = Password "username" "password"
--   illust <- runPixivT' credential $ getIllustDetail 70479649
--   print illust
-- @
-- This will print illustration information of <https://www.pixiv.net/artworks/70479649>.
-- 
-- More information is available on [README](https://github.com/The-closed-eye-of-love/pixiv).
module Web.Pixiv
  ( module Web.Pixiv.API,
    module Web.Pixiv.Types,
    module Web.Pixiv.Types.PixivT,
    Credential (Password),
  )
where

import Web.Pixiv.API
import Web.Pixiv.Auth
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivT
