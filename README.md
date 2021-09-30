# pixiv

[![build](https://github.com/The-closed-eye-of-love/pixiv/workflows/build/badge.svg)](https://github.com/The-closed-eye-of-love/pixiv/actions)
[![nix](https://github.com/The-closed-eye-of-love/pixiv/workflows/nix/badge.svg)](https://github.com/The-closed-eye-of-love/pixiv/actions)
[![Hackage](https://img.shields.io/hackage/v/pixiv.svg?logo=haskell)](https://hackage.haskell.org/package/pixiv)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/pixiv)
[![LICENSE](https://img.shields.io/github/license/The-closed-eye-of-love/pixiv)](LICENSE)

Haskell implementation of Pixiv API, based on [servant-client](https://hackage.haskell.org/package/servant-client).

## Usage

In most cases, it is enough to import only `Web.Pixiv`. If you want to use lenses to access and operate data types,
you should import `Web.Pixiv.Types.Lens` as well. Pixiv API requires authentication before being accessed, thus the `PixivT` monad transformer provides an user-friendly and thread-safe interface
to manage, and renew access token on demand, where you just need to give the ~~username and password~~ refresh token (See https://github.com/upbit/pixivpy/issues/158) of your pixiv account.

### Example

Here is a simpe example:

```haskell
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Web.Pixiv
import Web.Pixiv.Types.Lens

main :: IO ()
main = do
  let credential = RefreshToken "token"
  result <- runPixivT' credential action
  case result of
    Left err -> print err
    Right x -> pure x

action :: PixivT IO ()
action = do
  -- gets the details of user <https://www.pixiv.net/users/16731>
  userDetail <- getUserDetail 16731
  liftIO $ print userDetail

  -- gets the details of illustration <https://www.pixiv.net/artworks/80132896>
  illustDetail <- getIllustDetail 80132896
  liftIO $ print illustDetail

  -- gets day ranking illustrations
  -- 1 means the first page of the results
  ranking <- getIllustRanking (Just Day) 1
  liftIO $ print ranking

  -- searches the user who has name "玉之けだま" then gets their first work
  -- (function 'head' is not total, just used for demonstration) 
  targetUser <- head <$> searchUser "玉之けだま" Nothing 1
  firstWork <- head <$> getUserIllusts (targetUser ^. user . userId) (Just TypeIllust) 1
  liftIO $ print firstWork
```

As you can see, functions accessing Pixiv API are run in `PixivT IO` monad.
For more functionalities this library provides and relevant information about functions or data types, please refer to the documentation.  

## Documentation

Documentation is available at [hackage (latest release)](https://hackage.haskell.org/package/pixiv)
and our [github pages (master)](https://the-closed-eye-of-love.github.io/pixiv/).

## Related projects

* [pixivpy](https://github.com/upbit/pixivpy)
* [Pixiv-Shaft](https://github.com/CeuiLiSA/Pixiv-Shaft)
