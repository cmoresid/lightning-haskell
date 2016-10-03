{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Lightning.Plots.GraphBundled
  ( GraphBundledPlot(..)
  , Visualization (..)
  , graphBundledPlot
  , defGraphBundledPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities

data GraphBundledPlot =
  GraphBundledPlot { gpbX     :: [Double]
                   , gpbY     :: [Double]
                   , gpbConn  :: [[Double]]
                   , gpbSize  :: [Double]
                   , gpbGroup :: Maybe [Int] }
  deriving (Show, Eq)

instance Default GraphBundledPlot where
  def = GraphBundledPlot [] [] [[]] [] Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''GraphBundledPlot)

defGraphBundledPlot :: GraphBundledPlot
defGraphBundledPlot = def :: GraphBundledPlot

graphBundledPlot :: Monad m => GraphBundledPlot -> LightningT m Visualization
graphBundledPlot graphBPlt = sendPlot "graph" (transformData graphBPlt) R.plot

transformData :: GraphBundledPlot -> Value
transformData (GraphBundledPlot xs ys conn s g) =
  omitNulls [ "links" .= getLinks conn
            , "nodes" .= getPoints xs ys
            , "group" .= g
            , "size"  .= s ]
