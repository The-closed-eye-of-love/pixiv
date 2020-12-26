module Web.Pixiv.Types.User where

import Servant.API
import Web.Pixiv.Types
import Web.Pixiv.Types.Pixiv

type UserId = QueryParam' '[Required, Strict] "user_id" Int

type GetUserDetail =
  Pixiv :> "v1" :> "user" :> "detail"
    :> UserId
    :> Get '[JSON] UserDetail

type GetUserIllusts =
  Pixiv :> "v1" :> "user" :> "illusts"
    :> UserId
    :> Get '[JSON] Illusts

type GetUserFollowing =
  Pixiv :> "v1" :> "user" :> "following"
    :> UserId
    :> Get '[JSON] UserPreviews

type GetUserFollower =
  Pixiv :> "v1" :> "user" :> "follower"
    :> UserId
    :> Get '[JSON] UserPreviews

type GetUserMypixiv =
  Pixiv :> "v1" :> "user" :> "mypixiv"
    :> UserId
    :> Get '[JSON] UserPreviews
