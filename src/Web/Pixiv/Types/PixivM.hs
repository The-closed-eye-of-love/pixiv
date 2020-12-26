{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Pixiv.Types.PixivM where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Time
import GHC.Generics (Generic)
import Servant.Client
import Web.Pixiv.Auth

data TokenState = TokenState
  { accessToken :: Token,
    refreshToken :: Token,
    expirationTime :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

newtype PixivM a = PixivM
  { unPixivM :: ReaderT (MVar TokenState) ClientM a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader (MVar TokenState),
      MonadError ClientError,
      MonadBase IO,
      MonadBaseControl IO
    )
  deriving stock (Generic)

liftC :: ClientM a -> PixivM a
liftC = PixivM . lift

getAccessToken :: PixivM Token
getAccessToken = do
  ref <- ask
  s@TokenState {..} <- liftIO $ takeMVar ref
  t <- liftIO getCurrentTime
  if t < expirationTime
    then do
      liftIO $ putMVar ref s
      pure accessToken
    else do
      let credential = RefreshToken refreshToken
      authRes <- liftIO $ auth credential
      case authRes of
        AuthFailure err -> throwM err
        AuthSuccess OAuth2Token {..} -> do
          let offset = oa_expiresIn `div` 5 * 4
              diff = secondsToNominalDiffTime $ toEnum offset
              s' = s {expirationTime = addUTCTime diff t}
          liftIO $ putMVar ref s'
          pure accessToken
