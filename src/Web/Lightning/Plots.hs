{-|
Module      : Web.Lightning.Plots
Description : Lightning Plots
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX
-}

module Web.Lightning.Plots
  (
    -- * Sparse Adjacency Matrix
    AdjacencyPlot(..)
  , adjacencyPlot
    -- * Force-Directed Network
  , ForcePlot(..)
  , forcePlot
    -- * Graph
  , GraphPlot(..)
  , graphPlot
    -- * Graph Bundled
  , graphBundledPlot
    -- * Line
  , LinePlot(..)
  , linePlot
    -- * Map
  , MapPlot(..)
  , mapPlot
    -- * Matrix
  , MatrixPlot(..)
  , matrixPlot
    -- * Scatter
  , ScatterPlot(..)
  , scatterPlot
    -- * 3D Scatter
  , Scatter3Plot(..)
  , scatter3Plot

  , module Data.Default.Class
  )
  where

import           Data.Default.Class

import           Web.Lightning.Plots.Adjacency
import           Web.Lightning.Plots.Force
import           Web.Lightning.Plots.Graph
import           Web.Lightning.Plots.GraphBundled
import           Web.Lightning.Plots.Line
import           Web.Lightning.Plots.Map
import           Web.Lightning.Plots.Matrix
import           Web.Lightning.Plots.Scatter
import           Web.Lightning.Plots.Scatter3
