# lightning-haskell      [![Build Status](https://travis-ci.org/cmoresid/lightning-haskell.svg?branch=master)](https://travis-ci.org/cmoresid/lightning-haskell) [![Hackage](https://img.shields.io/hackage/v/lightning-haskell.svg)](http://hackage.haskell.org/package/lightning-haskell-0.1.0.3)

Haskell client for
[lightning-viz](http://lightning-viz.org/) server.  

### Getting Started
In order to use this package, a lightning-viz server will need to be available. If you are using Mac OS X, there is a stand-alone application you can install: [Stand-Alone OS X Application](http://lightning-viz.org/setup/#standalone).  

For installation on other platforms, please see the installation guide in the [lightnig-viz documentation](http://lightning-viz.org/setup/#prebuilt-server).

### Available Visualizations  
* **Adjacency** - A sparse adjacency matrix visualiazation.
* **Circle** - A circular graph from connectivity data.
* **Force** - A force-directed network visualization from connectivity.
* **Graph** - A node-link graph from spatial points and their connectivity.
* **Graph Bundled** - A bundled node-link graph visualization.
* **Histogram** - A distribution of values visualization.
* **Line** - Visualize one-dimensional series.
* **Streaming Line** - Visualize streaming one-dimensional series as updating lines.
* **Map** - A chloropleth map of the World or United States.
* **Matrix** - A heat map of the given matrix.
* **Scatter** - A scatter plot visualization.
* **Scatter 3D** - A 3D scatter plot visualization.
* **Streaming Scatter** - A streaming scatter plot.
* **Volume** - A visualization of a collection of images as a three-dimensional volume.

### Basic Usage  
All visualizations must be associated with a session. One can either use an existing session or create a new one. Sessions can either be named or un-named. If you choose not to provide a name, a name will be automatically generated.  

***Create Visualization Using New Session***
```haskell
viz <- runLightning $ linePlot def { lpSeries = [[1,2,3]] }
```

***Create Visualization Using Named Session***
```haskell
let opts = setSessionName "My Session" defaultLightningOptions
viz <- runLightningWith opts $ linePlot def { lpSeries = [[1,2,3,4]] }
```

***Create Multiple Visualizations In One Session***
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class

import qualified Data.Text.IO as T
import qualified Data.Text    as T

import Web.Lightning
import Web.Lightning.Plots
import Web.Lightning.Types

main :: IO (Either (APIError LightningError) ())
main = runLightning $ do
  lpViz <- linePlot def { lpSeries = [[1,2,3]] }
  spViz <- scatterPlot def { spX = [1,2,3], spY = [4,1,2] }
  liftIO $ T.putStrLn $ T.concat [vizId lpViz, ", ", vizId spViz]
```

### Acknowledgements
1. The monad transformer stack was based off of the stack found here: [reddit API](https://github.com/intolerable/reddit).
2. A fair amount of the code documentation was borrowed from the [lightning-viz Python client](http://lightning-viz.org/lightning-python/api.html).
