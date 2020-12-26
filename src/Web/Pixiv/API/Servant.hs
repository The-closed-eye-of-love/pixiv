module Web.Pixiv.API.Servant where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.Client
import Web.Pixiv.Auth (Token)
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.Search
import Web.Pixiv.Types.Trending
import Web.Pixiv.Types.User

getTrendingTags :: Token -> ClientM TrendingTags
getTrendingTags = client (Proxy @GetTrendingTags)

getIllustDetail :: Token -> Int -> ClientM IllustDetail
getIllustDetail = client (Proxy @GetIllustDetail)

getIllustComments :: Token -> Int -> Maybe Int -> ClientM Comments
getIllustComments = client (Proxy @GetIllustComments)

getIllustRelated :: Token -> Int -> Maybe Int -> ClientM Illusts
getIllustRelated = client (Proxy @GetIllustRelated)

searchIllust :: Token -> Text -> Maybe Bool -> Maybe SortingMethod -> Maybe Duration -> Maybe Int -> ClientM Illusts
searchIllust = client (Proxy @SearchIllust)

getUserDetail :: Token -> Int -> ClientM UserDetail
getUserDetail = client (Proxy @GetUserDetail)

getUserIllusts :: Token -> Int -> Maybe IllustType -> Maybe Int -> ClientM Illusts
getUserIllusts = client (Proxy @GetUserIllusts)

getUserFollowing :: Token -> Int -> Maybe Int -> ClientM UserPreviews
getUserFollowing = client (Proxy @GetUserFollowing)

getUserFollower :: Token -> Int -> Maybe Int -> ClientM UserPreviews
getUserFollower = client (Proxy @GetUserFollower)

getUserMypixiv :: Token -> Int -> Maybe Int -> ClientM UserPreviews
getUserMypixiv = client (Proxy @GetUserMypixiv)
