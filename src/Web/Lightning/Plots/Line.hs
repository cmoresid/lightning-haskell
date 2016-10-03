{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Line
  ( LinePlot(..)
  , Visualization (..)
  , linePlot
  , defLinePlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data LinePlot =
  LinePlot { lpSeries    :: Maybe [[Double]]
           , lpIndex     :: Maybe [Int]
           , lpThickness :: Maybe [Int]
           , lpXaxis     :: Maybe T.Text
           , lpYaxis     :: Maybe T.Text }
  deriving (Show, Eq)

instance Default LinePlot where
  def = LinePlot Nothing Nothing Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''LinePlot)

defLinePlot :: LinePlot
defLinePlot = def :: LinePlot

linePlot :: Monad m => LinePlot -> LightningT m Visualization
linePlot linePlt = sendPlot "line" linePlt R.plot
