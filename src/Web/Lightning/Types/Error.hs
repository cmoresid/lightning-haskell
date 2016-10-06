{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Types.Error
Description : Lightining error type
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Defines the Lightning error type.
-}

module Web.Lightning.Types.Error
  (
    -- Error Types
    LightningError(..)
  ) where

import Data.Aeson

import Network.API.Builder.Receive

-- | Represents a wrapped JSON error object.
data LightningError = LightningError Object
  deriving (Show)

instance FromJSON LightningError where
  parseJSON (Object o) = return $ LightningError o
  parseJSON _ = mempty

instance ErrorReceivable LightningError where
  receiveError = useErrorFromJSON
