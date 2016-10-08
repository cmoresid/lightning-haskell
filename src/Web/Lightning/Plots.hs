{-|
Module      : Web.Lightning.Plots
Description : Lightning Plots
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

This module re-exports all of the available plots so one does
not need to import each plot individually.
-}

module Web.Lightning.Plots (module P) where

import           Web.Lightning.Plots.Adjacency    as P
import           Web.Lightning.Plots.Force        as P
import           Web.Lightning.Plots.Graph        as P
import           Web.Lightning.Plots.GraphBundled as P
import           Web.Lightning.Plots.Line         as P
import           Web.Lightning.Plots.Map          as P
import           Web.Lightning.Plots.Matrix       as P
import           Web.Lightning.Plots.Scatter      as P
import           Web.Lightning.Plots.Scatter3     as P
import           Web.Lightning.Plots.ScatterLine  as P
