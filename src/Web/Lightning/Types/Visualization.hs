{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Types.Visualization
Description : Visualization type
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Describes the primary visualization return type along
with some helper functions.
-}

module Web.Lightning.Types.Visualization
  (
    -- * Visualization Types
    Visualization(..)
    -- * Visualization Helper Functions
  , getEmbedLink
  , getIFrameLink
  , getPymLink
  , getPublicLink
  )
  where

import           Data.Aeson
import qualified Data.Text           as T

import           Network.API.Builder hiding (runRoute)

-- | Encapsulates the basic information about a created
-- visualization.
data Visualization =
  Visualization { vizId        :: T.Text
                  -- ^ The unique identifier for a visualization
                , vizSessionID :: T.Text
                  -- ^ The session ID the visualization was created in
                , vizBaseUrl   :: Maybe T.Text
                  -- ^ Base URL gets filled in later
                } deriving (Show)

instance FromJSON Visualization where
  parseJSON (Object o) =
    Visualization <$> o .: "id"
                  <*> o .: "SessionId"
                  <*> o .:? "url"
  parseJSON _ = mempty

instance Receivable Visualization where
  receive = useFromJSON

-- | Appends a '/' to a URL if it is not there already.
formatURL :: T.Text
             -- ^ Base URL
          -> T.Text
             -- ^ Returns the formatted URL
formatURL url =
  case T.last url of
    '/' -> url
    _   -> T.concat [url, "/"]

-- | Returns the permanent link for a visualization.
getPermaLinkURL :: T.Text
                   -- ^ Base URL
                -> Visualization
                   -- ^ The visualization to get permalink for
                -> T.Text
                   -- ^ Returns the permalink for visualization.
getPermaLinkURL bUrl viz = T.concat [bUrl, "/visualizations/", vizId viz]

-- | Returns a partial URL based on the link type parameter.
getLinkType :: T.Text
               -- ^ Base URL
            -> Visualization
               -- ^ Visualization to create link for
            -> T.Text
               -- ^ The link type
            -> T.Text
               -- ^ The partially formatted URL
getLinkType bUrl viz link = formatURL $ T.concat [permaLink, link]
  where permaLink = getPermaLinkURL bUrl viz

-- | Returns the embedded link URL for a visualization.
getEmbedLink :: T.Text
                -- ^ Base URL
             -> Visualization
                -- ^ Visualization to create embedded link
             -> T.Text
                -- ^ Returns the embedded link for visualization
getEmbedLink bUrl viz = getLinkType bUrl viz "/embed"

-- | Returns the iFrame link URL for a visualization
getIFrameLink :: T.Text
                 -- ^ Base URL
              -> Visualization
                 -- ^ Visualization to create iFrame link for
              -> T.Text
                 -- ^ Returns the iFrame link for visualization
getIFrameLink bUrl viz = getLinkType bUrl viz "/iframe"

-- | Returns the PYM link for visualization.
getPymLink :: T.Text
              -- ^ Base URL
           -> Visualization
              -- ^ Visualization to create PYM link for
           -> T.Text
              -- ^ Returns the PYM link for visualization
getPymLink bUrl viz = getLinkType bUrl viz "/pym"

getPublicLink :: T.Text -> Visualization -> T.Text
getPublicLink bUrl viz = T.concat [getPermaLinkURL bUrl viz, "/public/"]
