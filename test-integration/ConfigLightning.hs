{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ConfigLightning where

import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.API.Builder

import System.Environment
import System.Exit

import Web.Lightning.Session
import Web.Lightning

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

newtype RunLightning = RunLightning
  { run :: forall a. Lightning a -> IO (Either (APIError LightningError) a) }

configLightning :: IO RunLightning
configLightning = do
  url <- getEnv "LIGHTNING_VIZ_URL"
  manager <- newManager tlsManagerSettings
  session <- interpretIO (LightningState (T.pack url) manager Nothing id) $ createSession (Just "Integration Tests")
  case session of
    Left  _ -> exitFailure
    Right s ->
      return $ RunLightning $ runLightningWith (LightningOptions (Just manager) (T.pack url) Anonymous (Just s))
