{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Routes
Description : Defines route mappings to lightning-viz API endpoints.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Defines the Route mappings to the lightning-viz API.
-}

module Web.Lightning.Routes
  ( -- * API Endpoints
    plot
  , stream
  , publicLink
  )
  where

--------------------------------------------------------------------------------
import           Network.API.Builder               hiding (runRoute)
import           Web.Lightning.Types.Visualization (Visualization (..))
--------------------------------------------------------------------------------

-- | The main route that corresponds to the /visualizations endpoint. Using
-- this route, one can create and update plots.
plot :: Route
        -- ^ Returns a new route corresponding to /sessionId/visualizations.
plot = Route ["visualizations"]
             []
             "POST"

-- | This route is used in conjunction with streaming plot functions.
stream :: Visualization
          -- ^ Visualization to append
       -> Route
          -- ^ Returns the URL to update the visualization.
stream (Visualization i _ _) =
  Route ["visualizations", i, "data"]
        []
        "POST"

-- | Corresponds to the public URL of a visualization.
publicLink :: Visualization
              -- ^ The visualization to create the public link for.
           -> Route
              -- ^ Route representing public link for visualization.
publicLink (Visualization i _ _) =
  Route ["visualizations", i, "public"]
        []
        "GET"
