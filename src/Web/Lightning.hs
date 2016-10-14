{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning
  ( LoginMethod(..)
  , LightningOptions(..)
  , LightningState(..)
  , runLightning
  , runLightningWith
  , runResumeLightningtWith
  -- * Client configuration
  , defaultLightningOptions
  , setSessionName
  , setSessionId
  -- * Re-export the following modules
  , APIError(..)
  , module Web.Lightning.Types.Error
  , module Web.Lightning.Types.Lightning
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                     as T

import           Web.Lightning.Types.Error
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Session
import           Web.Lightning.Utilities

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Network.API.Builder           as API

data LoginMethod = Anonymous
  deriving (Show)

instance Default LoginMethod where
  def = Anonymous

data LightningOptions =
  LightningOptions { optConnManager   :: Maybe Manager
                   , optHostUrl       :: T.Text
                   , optLoginMethod   :: LoginMethod
                   , optSession       :: Maybe Session }

instance Default LightningOptions where
  def = LightningOptions Nothing defaultBaseURL Anonymous Nothing

defaultLightningOptions :: LightningOptions
defaultLightningOptions = def

setSessionName :: T.Text -> LightningOptions -> LightningOptions
setSessionName n opts@(LightningOptions _ _ _ s) =
  opts { optSession = Just sess }
  where
    sess =
      case s of
        Nothing -> def { snName = Just n }
        Just s' -> s' { snName = Just n }

setSessionId :: T.Text -> LightningOptions -> LightningOptions
setSessionId i opts@(LightningOptions _ _ _ s) =
  opts { optSession = Just sess }
  where
    sess =
      case s of
        Nothing -> def { snId = i }
        Just s' -> s' { snId = i }

runLightning :: MonadIO m
             => LightningT m a
             -> m (Either (APIError LightningError) a)
runLightning = runLightningWith defaultLightningOptions

runLightningWith :: MonadIO m => LightningOptions
                     -> LightningT m a -> m (Either (APIError LightningError) a)
runLightningWith opts lightning =
  dropResume <$> runResumeLightningtWith opts lightning

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

runResumeLightningtWith :: MonadIO m => LightningOptions
                            -> LightningT m a
                            -> m (Either (APIError LightningError, Maybe (LightningT m a)) a)
runResumeLightningtWith (LightningOptions cm hu _ s) lightning = do
  manager <- case cm of
    Just m  -> return m
    Nothing -> liftIO $ newManager tlsManagerSettings
  session <- case s of
    Just s' ->
      case snId s' of
        "" -> (fmap . fmap) Just $
          interpretIO (LightningState hu manager Nothing) $ createSession $ snName s'
        _ -> return $ Right $ Just s'
    Nothing -> (fmap . fmap) Just $
      interpretIO (LightningState hu manager Nothing) $ createSession Nothing
  case session of
    Left (err, _) -> return $ Left (err, Just lightning)
    Right s' ->
      interpretIO (LightningState hu manager s') lightning

handleReceive :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> m (Either (APIError LightningError) a)
handleReceive r lstate = do
  (res, _, _) <- runAPI (builderFromState lstate) (stConnManager lstate) () $
    API.runRoute r

  return res

handleSendJSON :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> Value
                    -> m (Either (APIError LightningError) a)
handleSendJSON r lstate p = do
  (res, _, _) <- runAPI (builderFromState lstate) (stConnManager lstate) () $
    API.sendRoute p r

  return res

builderFromState :: LightningState -> Builder
builderFromState (LightningState hurl _ (Just s)) =
  Builder "Lightning" (addSessionId hurl (snId s)) id id
builderFromState (LightningState hurl _ Nothing) =
  Builder "Lightning" hurl id id

dropResume :: Either (APIError LightningError, Maybe (LightningT m a)) a
                    -> Either (APIError LightningError) a
dropResume (Left (x, _)) = Left x
dropResume (Right x)     = Right x

data LightningState =
  LightningState { stCurrentBaseURL :: T.Text
                 , stConnManager    :: Manager
                 , stSession        :: Maybe Session
                 }
