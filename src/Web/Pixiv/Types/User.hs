module Web.Pixiv.Types.User where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.PixivEntry

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
