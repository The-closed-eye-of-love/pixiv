{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Pixiv.Types.PixivT
  ( MonadPixiv (..),
    PixivState (..),
    TokenState (..),
    PixivT (..),
    ClientT (..),
    runClientT,
    liftC,
    computeTokenState,
    runPixivT,
    runPixivT',
    getAccessToken,
    getAccessTokenWithAccpetLanguage,
  )
where

import Control.Concurrent.MVar
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function ((&))
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import GHC.Show (showCommaSpace)
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
  deriving stock (Generic)

instance Show TokenState where
  showsPrec d TokenState {..} =
    showParen (d >= 11) $
      showString "TokenState {"
        . showString "accessToken = "
        . shows accessToken
        . showCommaSpace
        . showString "refreshToken = "
        . shows refreshToken
        . showCommaSpace
        . showString "expirationTime = "
        . shows expirationTime
        . showString "}"

data PixivState = PixivState
  { tokenState :: TokenState,
    acceptLanguage :: Maybe Text
  }
  deriving stock (Generic, Show)

newtype PixivT m a = PixivT
  { unPixivT :: ReaderT (MVar PixivState) (ClientT m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader (MVar PixivState),
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
  modifyPixivState :: (PixivState -> IO (PixivState, a)) -> m a

  modifyPixivState_ :: (PixivState -> IO PixivState) -> m ()
  modifyPixivState_ f = modifyPixivState $ f >=> (pure <$> (,()))

  -- | read the stored 'PixivState', when used in a multithreaded setting, this should block
  -- all other thread from reading the 'PixivState' until 'putPixivState' is called
  takePixivState :: m PixivState
  takePixivState = modifyPixivState $ \s -> pure (s, s)

  -- | write a new 'PixivState'
  putPixivState :: PixivState -> m ()
  putPixivState s = modifyPixivState_ $ \_ -> pure s

  takeTokenState :: m TokenState
  takeTokenState = tokenState <$> takePixivState

  putTokenState :: TokenState -> m ()
  putTokenState s = modifyPixivState_ (\p -> pure p {tokenState = s})

  takeAccpetLanguage :: m (Maybe Text)
  takeAccpetLanguage = acceptLanguage <$> takePixivState

  putAccpetLanguage :: Maybe Text -> m ()
  putAccpetLanguage lang = modifyPixivState_ (\p -> pure p {acceptLanguage = lang})

-- | A thread safe implementation of 'MonadPixiv'
instance MonadIO m => MonadPixiv (PixivT m) where
  modifyPixivState f = ask >>= liftIO . (`modifyMVar` f)
  takePixivState = ask >>= liftIO . takeMVar
  putPixivState s = do
    ref <- ask
    liftIO $ putMVar ref s

-- | Interprets the 'PixivT' effect, with a supplied 'Manager'
runPixivT :: MonadIO m => Manager -> Credential -> PixivT m a -> m (Either ClientError a)
runPixivT manager credential m = do
  t <- liftIO getCurrentTime
  s <- liftIO $ computeTokenState manager credential t
  clientEnv <- liftIO $ mkDefaultClientEnv manager
  ref <- liftIO . newMVar $ PixivState s Nothing
  m
    & unPixivT
    & flip runReaderT ref
    & runClientT clientEnv

-- | Like 'runPixivT', but creates a new 'Manager' everytime.
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

getAccessTokenWithAccpetLanguage :: MonadPixiv m => m (Token, Maybe Text)
getAccessTokenWithAccpetLanguage = (,) <$> getAccessToken <*> takeAccpetLanguage
