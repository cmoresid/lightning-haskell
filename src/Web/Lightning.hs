{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning
Description : lightning-viz REST API wrapper.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX
-}

module Web.Lightning
  (
    -- * Lightning Types
    LoginMethod(..)
  , LightningOptions(..)
  , LightningState(..)
    -- * Execute
  , runLightning
  , runLightningWith
  , runResumeLightningtWith
  -- * Client configuration
  , defaultLightningOptions
  , setSessionName
  , setSessionId
  , setBasicAuth
  -- * Re-exports
  , APIError(..)
  , module Web.Lightning.Types.Error
  , module Web.Lightning.Types.Lightning
  )
  where

--------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free

import           Data.Aeson
import qualified Data.ByteString               as B
import           Data.Default.Class
import qualified Data.Text                     as T

import           Web.Lightning.Session
import           Web.Lightning.Types.Error
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Utilities

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Network.API.Builder           as API
--------------------------------------------------------------------------------

-- | Represents the different authentication mechanisms available in
-- the lightning-viz server.
data LoginMethod = Anonymous -- ^ No authentication required
                 | BasicAuth Credentials -- ^ HTTP Basic Authentication
                 deriving (Show)

{- | Username and password pair for authenticating to the lightning-viz server. -}
type Credentials = (B.ByteString, B.ByteString)

instance Default LoginMethod where
  def = Anonymous

-- | Defines the available options for running a lightning action(s).
data LightningOptions =
  LightningOptions { optConnManager :: Maybe Manager
                     -- ^ Re-usable connection manager used during Lightning
                     -- session.
                   , optHostUrl     :: T.Text
                     -- ^ The base lightning-viz server url.
                   , optLoginMethod :: LoginMethod
                     -- ^ The authentication mechanism used to communicate
                     -- with the lightning-viz server.
                   , optSession     :: Maybe Session
                     -- ^ Defines what session to use when creating viaualizations
                     -- on the lightning-viz server. If no session is specified,
                     -- one will be created automatically.
                   }

instance Default LightningOptions where
  def = LightningOptions Nothing defaultBaseURL Anonymous Nothing

-- | Defines the default lightning-viz options.
defaultLightningOptions :: LightningOptions
defaultLightningOptions = def

-- | Sets the name of the session that is nested in the
-- given 'LightningOptions' record.
setSessionName :: T.Text
                  -- ^ The new session name
               -> LightningOptions
               -> LightningOptions
setSessionName n opts@(LightningOptions _ _ _ s) =
  opts { optSession = Just sess }
  where
    sess =
      case s of
        Nothing -> def { snName = Just n }
        Just s' -> s' { snName = Just n }

-- | Sets the session ID of the session nested in the given
-- 'LightningOptions' record.
setSessionId :: T.Text
                -- ^ The new session ID
             -> LightningOptions
             -> LightningOptions
setSessionId i opts@(LightningOptions _ _ _ s) =
  opts { optSession = Just sess }
  where
    sess =
      case s of
        Nothing -> def { snId = i }
        Just s' -> s' { snId = i }

-- | Sets 'BasicAuth' with 'Credentials' as the login method in the
-- given 'LightningOptions' record.
setBasicAuth :: Credentials -> LightningOptions -> LightningOptions
setBasicAuth creds opts = opts { optLoginMethod = BasicAuth creds }

-- | Performs a lightning action (or 'LightningT' transformer actions) with the
-- default lightning options. By default, the lightning-viz server is assumed to
-- be running on http://localhost:3000 and a new session will be created.
runLightning :: MonadIO m
             => LightningT m a
             -> m (Either (APIError LightningError) a)
runLightning = runLightningWith defaultLightningOptions

-- | Performs a lightning action (or 'LightningT' transformer actions) with
-- the specified lightning options.
runLightningWith :: MonadIO m => LightningOptions
                     -> LightningT m a
                     -> m (Either (APIError LightningError) a)
runLightningWith opts lightning =
  dropResume <$> runResumeLightningtWith opts lightning

-- | Runs a specified series of lightning actions.
interpretIO :: MonadIO m => LightningState
                -> LightningT m a
                -> m (Either (APIError LightningError, Maybe (LightningT m a)) a)
interpretIO lstate (LightningT r) =
  runFreeT r >>= \case
    Pure x -> return $ Right x
    Free (WithBaseURL u x n) ->
      interpretIO (lstate { stCurrentBaseURL = u }) x >>= \case
        Left (err, Just resume) ->
          return $ Left (err, Just $ resume >>= LightningT . n)
        Left (err, Nothing) -> return $ Left (err, Nothing)
        Right res -> interpretIO lstate $ LightningT $ n res
    Free (FailWith x) -> return $ Left (x, Nothing)
    Free (RunRoute route n) ->
      interpretIO lstate $ LightningT $ wrap $ ReceiveRoute route (n . unwrapJSON)
    Free (SendJSON jsonObj route n) ->
      handleSendJSON route lstate jsonObj >>= \case
        Left err -> return $ Left (err, Just $ LightningT $ wrap $ SendJSON jsonObj route n)
        Right x -> interpretIO lstate $ LightningT $ n x
    Free (ReceiveRoute route n) ->
      handleReceive route lstate >>= \case
        Left err -> return $ Left (err, Just $ LightningT $ wrap $ ReceiveRoute route n)
        Right x -> interpretIO lstate $ LightningT $ n x

-- | Runs a lightning action using the specified options.
runResumeLightningtWith :: MonadIO m => LightningOptions
                            -> LightningT m a
                            -> m (Either (APIError LightningError, Maybe (LightningT m a)) a)
runResumeLightningtWith (LightningOptions cm hu lm s) lightning = do
  manager <- case cm of
    Just m  -> return m
    Nothing -> liftIO $ newManager tlsManagerSettings
  auth <- case lm of
    Anonymous -> return id
    BasicAuth creds -> return $ uncurry applyBasicAuth creds
  session <- case s of
    Just s' ->
      case snId s' of
        "" -> (fmap . fmap) Just $
          interpretIO (LightningState hu manager Nothing auth) $ createSession $ snName s'
        _ -> return $ Right $ Just s'
    Nothing -> (fmap . fmap) Just $
      interpretIO (LightningState hu manager Nothing auth) $ createSession Nothing
  case session of
    Left (err, _) -> return $ Left (err, Just lightning)
    Right s' ->
      interpretIO (LightningState hu manager s' auth) lightning

-- | Runs the specified route using the current state.
handleReceive :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> m (Either (APIError LightningError) a)
handleReceive r lstate = do
  (res, _, _) <- runAPI (builderFromState lstate) (stConnManager lstate) () $
    API.runRoute r

  return res

-- | Runs the specified route and sends the specified JSON value as the the
-- request body.
handleSendJSON :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> Value
                    -> m (Either (APIError LightningError) a)
handleSendJSON r lstate p = do
  (res, _, _) <- runAPI (builderFromState lstate) (stConnManager lstate) () $
    API.sendRoute p r

  return res

-- | Creates a 'Builder' record to keep track of the lightning-viz server's
-- name and base URL.
builderFromState :: LightningState
                 -> Builder
builderFromState (LightningState hurl _ (Just s) auth) =
  Builder "Lightning" (addSessionId hurl (snId s)) id auth
builderFromState (LightningState hurl _ Nothing auth) =
  Builder "Lightning" hurl id auth

-- | Unwraps the response from interpretIO.
dropResume :: Either (APIError LightningError, Maybe (LightningT m a)) a
                    -> Either (APIError LightningError) a
dropResume (Left (x, _)) = Left x
dropResume (Right x)     = Right x

-- | Stores the current state of the lightning transformer stack.
data LightningState =
  LightningState { stCurrentBaseURL :: T.Text
                   -- ^ Current base URL of the lightning-viz server.
                 , stConnManager    :: Manager
                   -- ^ Current connection manager used to run actions.
                 , stSession        :: Maybe Session
                   -- ^ The current lightning session to run actions against.
                 , stApplyAuth      :: Request -> Request
                   -- ^ Hook to add auth credentials on performing a request
                   -- to the lightning-viz sever.
                 }
