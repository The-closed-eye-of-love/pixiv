module Web.Pixiv.API.Servant where

import Data.Proxy
import Data.Text (Text)
import Servant.Client
import Web.Pixiv.Types
import Web.Pixiv.Types.Illust
import Web.Pixiv.Types.Search
import Web.Pixiv.Types.Trending
import Web.Pixiv.Types.User

getTrendingTags :: Text -> ClientM TrendingTags
getTrendingTags = client (Proxy @GetTrendingTags)

getIllustDetail :: Text -> Int -> ClientM IllustDetail
getIllustDetail = client (Proxy @GetIllustDetail)

getIllustComments :: Text -> Int -> ClientM Comments
getIllustComments = client (Proxy @GetIllustComments)

getIllustRelated :: Text -> Int -> ClientM Illusts
getIllustRelated = client (Proxy @GetIllustRelated)

searchIllust :: Text -> Text -> Int -> Maybe Bool -> Maybe SortingMethod -> Maybe Duration -> ClientM Illusts
searchIllust = client (Proxy @SearchIllust)

getUserDetail :: Text -> Int -> ClientM UserDetail
getUserDetail = client (Proxy @GetUserDetail)

getUserIllusts :: Text -> Int -> ClientM Illusts
getUserIllusts = client (Proxy @GetUserIllusts)

getUserFollowing :: Text -> Int -> ClientM UserPreviews
getUserFollowing = client (Proxy @GetUserFollowing)

getUserFollower :: Text -> Int -> ClientM UserPreviews
getUserFollower = client (Proxy @GetUserFollower)

getUserMypixiv :: Text -> Int -> ClientM UserPreviews
getUserMypixiv = client (Proxy @GetUserMypixiv)
