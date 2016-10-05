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
  ) where

import           Network.API.Builder hiding (runRoute)

-- | The main route that corresponds to the /visualizations endpoint. Using
-- this route, one can create and update plots.
plot :: Route
        -- ^ Returns a new route corresponding to /sessionId/visualizations.
plot = Route ["visualizations"]
             []
             "POST"
