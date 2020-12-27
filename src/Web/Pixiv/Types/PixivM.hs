{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Pixiv.Types.PixivM where

{-
  ( PixivM,
    runPixivM,
    runPixivM',
    liftC,
    getAccessToken,
  )
-}

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function ((&))
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Internal.HttpClient
import Web.Pixiv.Auth
import Web.Pixiv.Utils

newtype ClientT m a = ClientT
  { unClientT :: ReaderT ClientEnv (ExceptT ClientError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader ClientEnv,
      MonadError ClientError
    )

instance MonadTrans ClientT where
  lift = ClientT . lift . lift

runClientT :: ClientEnv -> ClientT m a -> m (Either ClientError a)
runClientT env m =
  m
    & unClientT
    & flip runReaderT env
    & runExceptT

deriving newtype instance MonadBase IO m => MonadBase IO (ClientT m)

deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (ClientT m)

instance MonadIO m => RunClient (ClientT m) where
  throwClientError = throwError
  runRequestAcceptStatus status req = do
    env <- ask
    let m = performRequest status req
    res <- liftIO $ runClientM m env
    liftEither res

data TokenState = TokenState
  { accessToken :: Token,
    refreshToken :: Token,
    expirationTime :: UTCTime,
    manager :: Manager
  }

newtype PixivT m a = PixivT
  { unPixivT :: ReaderT (MVar TokenState) (ClientT m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader (MVar TokenState),
      MonadError ClientError
    )
  deriving stock (Generic)

instance MonadTrans PixivT where
  lift = PixivT . lift . lift

deriving newtype instance MonadBase IO m => MonadBase IO (PixivT m)

deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (PixivT m)

liftC :: Monad m => ClientT m a -> PixivT m a
liftC = PixivT . lift

instance MonadIO m => RunClient (PixivT m) where
  throwClientError = throwError
  runRequestAcceptStatus status req =
    liftC $ runRequestAcceptStatus status req

class (RunClient m, MonadIO m) => MonadPixiv m where
  takeTokenState :: m TokenState
  putTokenState :: TokenState -> m ()

instance MonadIO m => MonadPixiv (PixivT m) where
  takeTokenState = ask >>= liftIO . takeMVar
  putTokenState s = do
    ref <- ask
    liftIO $ putMVar ref s

runPixivT :: MonadIO m => Manager -> Credential -> PixivT m a -> m (Either ClientError a)
runPixivT manager credential m = do
  t <- liftIO getCurrentTime
  s <- liftIO $ computeTokenState manager credential t
  clientEnv <- liftIO $ mkDefaultClientEnv manager
  ref <- liftIO $ newMVar s
  m
    & unPixivT
    & flip runReaderT ref
    & runClientT clientEnv

-- | Like 'runPixivT', but creates a new 'Manager' everythime.
runPixivT' :: MonadIO m => Credential -> PixivT m a -> m (Either ClientError a)
runPixivT' credential m = do
  manager <- liftIO newTlsManager
  runPixivT manager credential m

computeTokenState :: Manager -> Credential -> UTCTime -> IO TokenState
computeTokenState manager credential time = do
  OAuth2Token {..} <- liftIO $ auth' manager credential
  let offset = oa_expiresIn `div` 5 * 4
      diff = secondsToNominalDiffTime $ toEnum offset
      accessToken = oa_accessToken
      refreshToken = oa_refreshToken
      expirationTime = addUTCTime diff time
  pure TokenState {..}

getAccessToken :: MonadPixiv m => m Token
getAccessToken = do
  s@TokenState {..} <- takeTokenState
  t <- liftIO getCurrentTime
  if t < expirationTime
    then do
      putTokenState s
      pure accessToken
    else do
      let credential = RefreshToken refreshToken
      s' <- liftIO $ computeTokenState manager credential t
      putTokenState s'
      pure accessToken
