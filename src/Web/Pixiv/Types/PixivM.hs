{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Pixiv.Types.PixivM
  ( PixivM,
    runPixivM,
    runPixivM',
    liftC,
    getAccessToken,
  )
where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function ((&))
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
import Web.Pixiv.Auth
import Web.Pixiv.Utils

data TokenState = TokenState
  { accessToken :: Token,
    refreshToken :: Token,
    expirationTime :: UTCTime,
    manager :: Manager
  }

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

runPixivM :: Manager -> Credential -> PixivM a -> IO (Either ClientError a)
runPixivM manager credential m = do
  t <- liftIO getCurrentTime
  s <- computeTokenState manager credential t
  clientEnv <- mkDefaultClientEnv manager
  ref <- newMVar s
  m
    & unPixivM
    & flip runReaderT ref
    & flip runClientM clientEnv

-- | Like 'runPixivM', but creates a new 'Manager' everythime.
runPixivM' :: Credential -> PixivM a -> IO (Either ClientError a)
runPixivM' credential m = do
  manager <- newTlsManager
  runPixivM manager credential m

computeTokenState :: Manager -> Credential -> UTCTime -> IO TokenState
computeTokenState manager credential time = do
  OAuth2Token {..} <- liftIO $ auth' manager credential
  let offset = oa_expiresIn `div` 5 * 4
      diff = secondsToNominalDiffTime $ toEnum offset
      accessToken = oa_accessToken
      refreshToken = oa_refreshToken
      expirationTime = addUTCTime diff time
  pure TokenState {..}

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
      s' <- liftIO $ computeTokenState manager credential t
      liftIO $ putMVar ref s'
      pure accessToken
