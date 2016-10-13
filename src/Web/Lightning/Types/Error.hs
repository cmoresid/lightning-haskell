{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Types.Error
Description : Lightning error type
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX
-}

module Web.Lightning.Types.Error
  (
    -- Error Types
    LightningError(..)
  ) where

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text                   as T

import           Network.API.Builder.Receive
--------------------------------------------------------------------------------

-- | Represents the different errors that may be raised in the lightning-viz
-- wrapper.
data LightningError = LightningError  Object
                      -- ^ Represents a JSON error returned by lightning-viz.
                    | FailError       T.Text
                      -- ^ Represents a generic exception error.
                    | ValidationError T.Text
                      -- ^ Represents a validation error in a request record.
                    deriving (Show, Eq)

instance FromJSON LightningError where
  parseJSON (Object o) = return $ LightningError o
  parseJSON _          = mempty

instance ErrorReceivable LightningError where
  receiveError = useErrorFromJSON
