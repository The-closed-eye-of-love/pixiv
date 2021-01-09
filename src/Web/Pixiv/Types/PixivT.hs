{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Copyright: (c) 2021 The closed eye of love
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Poscat <poscat@mail.poscat.moe>, berberman <berberman@yandex.com>
-- Stability: alpha
-- Portability: portable
-- The core monad of this library. 'PixivT' maintains a pixiv login state,
-- and provides an environment to perform computations created by servant.
module Web.Pixiv.Types.PixivT
  ( -- * ClientT monad transformer
    ClientT (..),
    runClientT,
    mkDefaultClientEnv,

    -- * MonadPixiv class
    MonadPixiv (..),
    PixivState (..),

    -- * PixivT monad transformer
    PixivT (..),
    liftC,
    runPixivT,
    runPixivT',

    -- * Token
    TokenState (..),
    computeTokenState,

    -- * Utilities
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

-- | Transformer version of 'ClientM', changing the base 'IO' to @m@.
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
      MonadError ClientError,
      MonadBase b,
      MonadBaseControl b
    )

instance MonadTrans ClientT where
  lift = ClientT . lift . lift

-- | Executes a computation in the client monad.
runClientT :: ClientEnv -> ClientT m a -> m (Either ClientError a)
runClientT env m =
  m
    & unClientT
    & flip runReaderT env
    & runExceptT

instance MonadIO m => RunClient (ClientT m) where
  throwClientError = throwError
  runRequestAcceptStatus status req = do
    env <- ask
    let m = performRequest status req
    res <- liftIO $ runClientM m env
    liftEither res

-- | Given 'Manager', creates a 'ClientEnv' using @"https://app-api.pixiv.net"@ as base url.
mkDefaultClientEnv :: Manager -> IO ClientEnv
mkDefaultClientEnv manager = do
  baseUrl <- parseBaseUrl "https://app-api.pixiv.net"
  pure $ mkClientEnv manager baseUrl

-- | Pixiv auth state.
data TokenState = TokenState
  { -- | Token to access pixiv api.
    accessToken :: Token,
    -- | Token to obtain new 'accessToken' without giving username and password.
    refreshToken :: Token,
    -- | Time stamp when /both/ 'accessToken' and 'refreshToken' are invalid.
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

-- | State stored in 'MonadPixiv'.
data PixivState = PixivState
  { tokenState :: TokenState,
    acceptLanguage :: Maybe Text
  }
  deriving stock (Generic, Show)

-- | A thread safe implementation of 'MonadPixiv'.
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

-- | Lifts a computation in 'ClientT' to 'PixivT'.
liftC :: Monad m => ClientT m a -> PixivT m a
liftC = PixivT . lift

instance MonadIO m => RunClient (PixivT m) where
  throwClientError = throwError
  runRequestAcceptStatus status req =
    liftC $ runRequestAcceptStatus status req

-- | The mtl-style class of pixiv monad.
class (RunClient m, MonadIO m) => MonadPixiv m where
  -- | Reads the stored 'PixivState', when used in a multithreaded setting, this should block
  -- all other thread from reading the 'PixivState' until 'putPixivState' is called.
  takePixivState :: m PixivState

  -- | Writes a new 'PixivState'.
  putPixivState :: PixivState -> m ()

  -- | Reads the stored 'PixivState', without blocking other threads which want to read this state.
  --
  -- Don't confuse with 'takePixivState', please refer to 'readMVar'.
  readPixivState :: m PixivState

instance
  {-# OVERLAPPABLE #-}
  ( MonadPixiv m,
    MonadTrans f,
    MonadIO (f m),
    RunClient (f m)
  ) =>
  MonadPixiv (f m)
  where
  takePixivState = lift takePixivState
  putPixivState = lift . putPixivState
  readPixivState = lift readPixivState

instance MonadIO m => MonadPixiv (PixivT m) where
  takePixivState = ask >>= liftIO . takeMVar
  putPixivState s = do
    ref <- ask
    liftIO $ putMVar ref s
  readPixivState = ask >>= liftIO . readMVar

-- | Interprets the 'PixivT' effect, with a supplied 'Manager'.
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

-- | Computes the 'TokenState'.
computeTokenState ::
  Manager ->
  -- | Could be username with password or 'refreshToken'.
  Credential ->
  -- | Current time.
  UTCTime ->
  IO TokenState
computeTokenState manager credential time = do
  OAuth2Token {..} <- liftIO $ auth' manager credential
  let offset = oa_expiresIn `div` 5 * 4
      diff = secondsToNominalDiffTime $ toEnum offset
      accessToken = oa_accessToken
      refreshToken = oa_refreshToken
      expirationTime = addUTCTime diff time
  pure TokenState {..}

-- | Retrieves the 'accessToken' from pixiv monad.
--
-- If the token is overdue, it will call 'computeTokenState' to refresh.
getAccessToken :: MonadPixiv m => m Token
getAccessToken = do
  s@PixivState {tokenState = TokenState {..}} <- takePixivState
  t <- liftIO getCurrentTime
  if t < expirationTime
    then do
      putPixivState s
      pure accessToken
    else do
      let credential = RefreshToken refreshToken
      ts <- liftIO $ computeTokenState manager credential t
      putPixivState s {tokenState = ts}
      pure accessToken

-- | Retrieves the 'acceptLanguage' from pixiv monad.
getAccpetLanguage :: MonadPixiv m => m (Maybe Text)
getAccpetLanguage = acceptLanguage <$> readPixivState

-- | Retrieves the 'accessToken' and 'acceptLanguage' in one go.
getAccessTokenWithAccpetLanguage :: MonadPixiv m => m (Token, Maybe Text)
getAccessTokenWithAccpetLanguage = (,) <$> getAccessToken <*> getAccpetLanguage
