module Web.Pixiv.API.User
  ( module Web.Pixiv.API.User,
  )
where

import Servant.API
import Web.Pixiv.API.PixivEntry
import Web.Pixiv.Types

type UserIdParam = QueryParam' '[Required, Strict] "user_id" Int

type GetUserDetail =
  PixivEntry :> "v1" :> "user" :> "detail"
    :> UserIdParam
    :> Get '[JSON] UserDetail

type GetUserIllusts =
  PixivEntry :> "v1" :> "user" :> "illusts"
    :> UserIdParam
    :> QueryParam "type" IllustType
    :> OffsetParam
    :> Get '[JSON] Illusts

type GetUserFollowing =
  PixivEntry :> "v1" :> "user" :> "following"
    :> UserIdParam
    :> RestrictParam
    :> OffsetParam
    :> Get '[JSON] UserPreviews

type GetUserFollower =
  PixivEntry :> "v1" :> "user" :> "follower"
    :> UserIdParam
    :> OffsetParam
    :> Get '[JSON] UserPreviews

type GetUserMypixiv =
  PixivEntry :> "v1" :> "user" :> "mypixiv"
    :> UserIdParam
    :> OffsetParam
    :> Get '[JSON] UserPreviews

type GetUserBookmarks =
  PixivEntry :> "v1" :> "user" :> "bookmarks" :> "illust"
    :> UserIdParam
    :> RestrictParam
    :> QueryParam "max_bookmark_id" Int
    :> Get '[JSON] Illusts
