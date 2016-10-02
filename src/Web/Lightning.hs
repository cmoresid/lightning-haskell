{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free

import Data.Aeson
import           Data.Default.Class
import qualified Data.Text                     as T

import           Web.Lightning.Types.Error
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Utilities

import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Client.TLS

import           Network.API.Builder           as API

data LoginMethod = Anonymous
  deriving (Show)

instance Default LoginMethod where
  def = Anonymous

data LightningOptions =
  LightningOptions { connectionManager :: Maybe Manager
                   , hostUrl           :: T.Text
                   , loginMethod       :: LoginMethod
                   , sessionId         :: Maybe T.Text }

instance Default LightningOptions where
  def = LightningOptions Nothing defaultBaseURL Anonymous Nothing

runLightningAnon :: MonadIO m => LightningT m a -> m (Either (APIError LightningError) a)
runLightningAnon = runLightningWith def

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
      interpretIO (lstate { currentBaseURL = u }) x >>= \case
        Left (err, Just resume) ->
          return $ Left (err, Just $ resume >>= LightningT . n)
        Left (err, Nothing) -> return $ Left (err, Nothing)
        Right res -> interpretIO lstate $ LightningT $ n res
    Free (FailWith x) -> return $ Left (x, Nothing)
    Free (RunRoute route n) ->
      interpretIO lstate $ LightningT $ wrap $ ReceiveRoute route (n . unwrapJSON)
    Free (SendPlot t obj route n) ->
      handlePlot route lstate (createPayLoad t obj) >>= \case
        Left err -> return $ Left (err, Just $ LightningT $ wrap $ SendPlot t obj route n)
        Right x -> interpretIO lstate $ LightningT $ n x
    Free (ReceiveRoute route n) ->
      handleReceive route lstate >>= \case
        Left err -> return $ Left (err, Just $ LightningT $ wrap $ ReceiveRoute route n)
        Right x -> interpretIO lstate $ LightningT $ n x

runResumeLightningtWith :: MonadIO m => LightningOptions
                            -> LightningT m a
                            -> m (Either (APIError LightningError, Maybe (LightningT m a)) a)
runResumeLightningtWith (LightningOptions cm hu lm s) lightning = do
  manager <- case cm of
    Just m -> return m
    Nothing -> liftIO $ newManager tlsManagerSettings
  loginCreds <- case lm of
    Anonymous -> return $ Right Nothing
  case loginCreds of
    Left (err, _) -> return $ Left (err, Just lightning)
    Right _ ->
      interpretIO (LightningState hu manager s) lightning

handleReceive :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> m (Either (APIError LightningError) a)
handleReceive r lstate = do
  (res, _, _) <- runAPI (builderFromState lstate) (connMgr lstate) () $
    API.runRoute r
  return res

handlePlot :: (MonadIO m, Receivable a) => Route
                    -> LightningState
                    -> Value
                    -> m (Either (APIError LightningError) a)
handlePlot r lstate p = do
  (res, _, _) <- runAPI (builderFromState lstate) (connMgr lstate) () $
    API.sendRoute p r
  return res

builderFromState :: LightningState -> Builder
builderFromState (LightningState hurl _ (Just s)) =
  Builder "Lightning" (addSessionId hurl s) id id
builderFromState (LightningState hurl _ Nothing) =
  Builder "Lightning" hurl id id

dropResume :: Either (APIError LightningError, Maybe (LightningT m a)) a
                    -> Either (APIError LightningError) a
dropResume (Left (x, _)) = Left x
dropResume (Right x) = Right x

data LightningState =
  LightningState { currentBaseURL :: T.Text
                 , connMgr        :: Manager
                 , _sessionId     :: Maybe T.Text
                 }

addHeaders :: [Header] -> Request -> Request
addHeaders xs req = req { requestHeaders = requestHeaders req ++ xs }