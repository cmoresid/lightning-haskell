{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Session
Description : Session management.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Defines interactions with the session endpoint.
-}

module Web.Lightning.Session
  (
    -- * Session Functions
    createSession
  , module Web.Lightning.Types.Session
  ) where

import qualified Data.Text                     as T

import           Data.Maybe

import           Network.API.Builder           hiding (runRoute)

import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types.Session


-- | Session endpoint.
createSessionRoute :: Maybe T.Text
                      -- ^ An optional name for the session
                   -> Route
                      -- ^ The lightning-viz session endpoint
createSessionRoute n = Route ["sessions"]
                             ["name" =. fromMaybe "" n]
                             "POST"

-- | Creates a new session with an optional name.
createSession :: Monad m => Maybe T.Text
                            -- ^ An optional session name
                         -> LightningT m Session
                            -- ^ Returns the LightningT transformer stack
createSession n = receiveRoute $ createSessionRoute n
