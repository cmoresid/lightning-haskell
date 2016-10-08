{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Plots.Graph
  ( GraphPlot(..)
  , Visualization (..)
  , graphPlot
  , defGraphPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities

data GraphPlot =
  GraphPlot { gpX     :: [Double]
            , gpY     :: [Double]
            , gpConn  :: [[Double]]
            , gpSize  :: [Double]
            , gpGroup :: Maybe [Int] }
  deriving (Show, Eq)

instance Default GraphPlot where
  def = GraphPlot [] [] [[]] [] Nothing

defGraphPlot :: GraphPlot
defGraphPlot = def :: GraphPlot

graphPlot :: Monad m => GraphPlot -> LightningT m Visualization
graphPlot graphPlt = sendPlot "graph" (transformData graphPlt) R.plot

transformData :: GraphPlot -> Value
transformData (GraphPlot xs ys conn s g) =
  omitNulls [ "links" .= getLinks conn
            , "nodes" .= getPoints xs ys
            , "group" .= g
            , "size"  .= s ]
