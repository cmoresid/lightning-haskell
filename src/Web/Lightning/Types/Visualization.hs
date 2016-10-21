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

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text                     as T

import           Network.API.Builder           hiding (runRoute)
import           Web.Lightning.Utilities       (defaultBaseURL)
--------------------------------------------------------------------------------

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
getPermaLinkURL :: Visualization
                   -- ^ The visualization to get permalink for
                -> T.Text
                   -- ^ Returns the permalink for visualization.
getPermaLinkURL (Visualization i _ (Just bUrl)) =
  T.concat [formatURL bUrl, "visualizations/", i]
getPermaLinkURL (Visualization i _ Nothing) =
  T.concat [formatURL defaultBaseURL, "visualizations/", i]

-- | Returns a partial URL based on the link type parameter.
getLinkType :: Visualization
               -- ^ Visualization to create link for
            -> T.Text
               -- ^ The link type
            -> T.Text
               -- ^ The partially formatted URL
getLinkType viz link = formatURL $ T.concat [permaLink, link]
  where permaLink = getPermaLinkURL viz

-- | Returns the embedded link URL for a visualization.
getEmbedLink :: Visualization
                -- ^ Visualization to create embedded link
             -> T.Text
                -- ^ Returns the embedded link for visualization
getEmbedLink viz = getLinkType viz "/embed/"

-- | Returns the iFrame link URL for a visualization
getIFrameLink :: Visualization
                 -- ^ Visualization to create iFrame link for
              -> T.Text
                 -- ^ Returns the iFrame link for visualization
getIFrameLink viz = getLinkType viz "/iframe/"

-- | Returns the PYM link for visualization.
getPymLink :: Visualization
              -- ^ Visualization to create PYM link for
           -> T.Text
              -- ^ Returns the PYM link for visualization
getPymLink viz = getLinkType viz "/pym/"

-- | Returns the public link for a visualization.
getPublicLink :: Visualization
                 -- ^ Visualization to create public link for.
              -> T.Text
                 -- ^ Returns the public link for visualization.
getPublicLink viz = getLinkType viz "/public/"
