{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Session
  ( createSession
  , module Web.Lightning.Types.Session) where

import qualified Data.Text as T

import Data.Maybe

import Network.API.Builder hiding (runRoute)

import Web.Lightning.Types.Lightning
import Web.Lightning.Types.Session


createSessionRoute :: Maybe T.Text -> Route
createSessionRoute n = Route ["sessions"]
                             ["name" =. fromMaybe "" n]
                             "POST"

createSession :: Monad m => Maybe T.Text -> LightningT m Session
createSession n = receiveRoute $ createSessionRoute n
