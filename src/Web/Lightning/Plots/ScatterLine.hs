{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Lightning.Plots.ScatterLine
  ( ScatterLinePlot(..)
  , Visualization (..)
  , scatterLinePlot
  , defScatterLinePlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data ScatterLinePlot =
  ScatterLinePlot { slpX      :: Maybe [Double]
                  , slpY      :: Maybe [Double]
                  , slpSeries :: Maybe [[Double]]
                  , slpLabel  :: Maybe [Double]
                  , slpSize   :: Maybe [Double]
                  , slpAlpha  :: Maybe [Double] }
  deriving (Show, Eq)

instance Default ScatterLinePlot where
  def = ScatterLinePlot Nothing Nothing Nothing Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''ScatterLinePlot)

defScatterLinePlot :: ScatterLinePlot
defScatterLinePlot = def :: ScatterLinePlot

scatterLinePlot :: Monad m => ScatterLinePlot -> LightningT m Visualization
scatterLinePlot linePlt = sendPlot "line" linePlt R.plot
