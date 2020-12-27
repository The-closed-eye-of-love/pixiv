module Web.Pixiv.API.Servant
  ( -- * Trending
    getTrendingTags,
    getRecommendedIllusts,
    getRecommendedMangas,

    -- * Illust
    getIllustDetail,
    getIllustComments,
    getIllustRelated,
    getIllustRanking,
    getIllustFollow,
    getIllustNew,

    -- * Search
    searchIllust,

    -- * User
    getUserDetail,
    getUserIllusts,
    getUserFollowing,
    getUserFollower,
    getUserMypixiv,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.Client
import Web.Pixiv.Auth (Token)
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.Search
import Web.Pixiv.Types.Trending
import Web.Pixiv.Types.User

-----------------------------------------------------------------------------

getTrendingTags :: Token -> Maybe Bool -> ClientM TrendingTags
getTrendingTags = client (Proxy @GetTrendingTags)

getRecommendedIllusts :: Token -> Maybe Bool -> Maybe Bool -> ClientM Illusts
getRecommendedIllusts = client (Proxy @GetRecommendedIllusts)

getRecommendedMangas :: Token -> Maybe Bool -> Maybe Bool -> ClientM Illusts
getRecommendedMangas = client (Proxy @GetRecommendedMangas)

-----------------------------------------------------------------------------

getIllustDetail :: Token -> Int -> ClientM IllustDetail
getIllustDetail = client (Proxy @GetIllustDetail)

getIllustComments :: Token -> Int -> Maybe Int -> ClientM Comments
getIllustComments = client (Proxy @GetIllustComments)

getIllustRelated :: Token -> Int -> Maybe Int -> ClientM Illusts
getIllustRelated = client (Proxy @GetIllustRelated)

getIllustRanking :: Token -> Maybe RankMode -> Maybe Int -> ClientM Illusts
getIllustRanking = client (Proxy @GetIllustRanking)

getIllustFollow :: Token -> Maybe Publicity -> Maybe Int -> ClientM Illusts
getIllustFollow = client (Proxy @GetIllustFollow)

getIllustNew :: Token -> Maybe Int -> ClientM Illusts
getIllustNew = client (Proxy @GetIllustNew)

-----------------------------------------------------------------------------

searchIllust ::
  Token ->
  SearchTarget ->
  Text ->
  Maybe Bool ->
  Maybe SortingMethod ->
  Maybe Duration ->
  Maybe Int ->
  ClientM Illusts
searchIllust = client (Proxy @SearchIllust)

-----------------------------------------------------------------------------

getUserDetail :: Token -> Int -> ClientM UserDetail
getUserDetail = client (Proxy @GetUserDetail)

getUserIllusts :: Token -> Int -> Maybe IllustType -> Maybe Int -> ClientM Illusts
getUserIllusts = client (Proxy @GetUserIllusts)

getUserFollowing :: Token -> Int -> Maybe Publicity -> Maybe Int -> ClientM UserPreviews
getUserFollowing = client (Proxy @GetUserFollowing)

getUserFollower :: Token -> Int -> Maybe Int -> ClientM UserPreviews
getUserFollower = client (Proxy @GetUserFollower)

getUserMypixiv :: Token -> Int -> Maybe Int -> ClientM UserPreviews
getUserMypixiv = client (Proxy @GetUserMypixiv)
