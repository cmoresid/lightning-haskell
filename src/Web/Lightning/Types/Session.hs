{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Default.Class
import qualified Data.Text           as T

import           Network.API.Builder hiding (runRoute)

-- | Represents a lightning-viz session. A session ID is required to create
-- a plot.
data Session =
  Session { snId   :: T.Text
            -- ^ The unique session ID
          , snName :: Maybe T.Text
            -- ^ The optional session name
          , snUpdated   :: Maybe T.Text
            -- ^ The timestamp of when the session was last updated
          , snCreated   :: Maybe T.Text
            -- ^ Creation timestamp
          }
  deriving (Show, Read, Eq)

instance FromJSON Session where
  parseJSON (Object v) =
    Session <$>
    v .: "id" <*>
    v .: "name" <*>
    v .: "updatedAt" <*>
    v .: "createdAt"
  parseJSON _ = mempty

instance Receivable Session where
  receive = useFromJSON

instance Default Session where
  def = Session "" Nothing Nothing Nothing
