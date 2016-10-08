{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Web.Lightning.Types.Session
Description : Session management types.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Defines the types needed for interaction with the session endpoint.
-}

module Web.Lightning.Types.Session
  (
    -- * Session Types
    Session(..)
  )
  where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text           as T

import           Network.API.Builder hiding (runRoute)

-- | Represents a lightning-viz session. A session ID is required to create
-- a plot.
data Session =
  Session { sessionID   :: T.Text
            -- ^ The unique session ID
          , sessionName :: T.Text
            -- ^ The optional session name
          , updatedAt   :: T.Text
            -- ^ The timestamp of when the session was last updated
          , createdAt   :: T.Text
            -- ^ Creation timestamp
          }
  deriving (Show, Read, Eq, Ord)

$(deriveFromJSON defaultOptions { omitNothingFields = True} ''Session)

instance Receivable Session where
  receive = useFromJSON
