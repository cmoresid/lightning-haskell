{-# LANGUAGE LambdaCase #-}

module Web.Lightning where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free

import Data.Default.Class
import qualified Data.Text as T

import Web.Lightning.Types.Lightning
import Web.Lightning.Types.Error

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Network.API.Builder as API

data LoginMethod = Anonymous
                 | Credentials T.Text T.Text
  deriving (Show)

instance Default LoginMethod where
  def = Anonymous

data LightningOptions =
  LightningOptions { connectionManager :: Maybe Manager
                   , loginMethod :: LoginMethod }

instance Default LightningOptions where
  def = LightningOptions Nothing Anonymous

defaultLightningOptions :: LightningOptions
defaultLightningOptions = def

--runLightningWith :: MonadIO m => LightningOptions -> LightningT m a -> m (Either (APIError LightningError) a)
--runLightningWith opts lightning = liftM dropResume

{-interpretIO :: MonadIO m => LightningState -> LightningT m a -> m (Either (APIError LightningError, Maybe (LightningT m a)) a)
interpretIO lstate (LightningT r) =
  runFreeT r >>= \case
    Pure x -> return $ Right x
    Free (WithBaseURL u x n) ->
      interpretIO (lstate )-}

dropResume :: Either (APIError LightningError, Maybe (LightningT m a)) a -> Either (APIError LightningError) a
dropResume (Left (x, _)) = Left x
dropResume (Right x) = Right x

data LightningState =
  LightningState { currentBaseURL :: T.Text
                 , connMgr :: Manager
                 , sessionID :: T.Text
                 }

addHeaders :: [Header] -> Request -> Request
addHeaders xs req = req { requestHeaders = requestHeaders req ++ xs }
