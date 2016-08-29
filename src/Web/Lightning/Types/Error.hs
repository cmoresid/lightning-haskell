{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.Error where

import Data.Aeson
import qualified Data.Text as T

import Network.API.Builder.Receive

data LightningError = LightningError Object
  | FailError T.Text
  | CredentialsError

instance FromJSON LightningError where
  parseJSON (Object o) = return $ LightningError o
  parseJSON _ = mempty

instance ErrorReceivable LightningError where
  receiveError = useErrorFromJSON
