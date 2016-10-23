{-# LANGUAGE OverloadedStrings #-}

-- | Visualize one-dimensional series data as lines.
module Web.Lightning.Plots.Histogram
  (
    HistogramPlot(..)
  , Visualization (..)
  , histogramPlot
  )
  where

--------------------------------------------------------------------------------
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities
--------------------------------------------------------------------------------

-- | Histogram plot parameters
data HistogramPlot =
  HistogramPlot { hpValues :: [Double]
                , hpBins   :: Maybe [Double]
                , hpZoom   :: Maybe Bool
                }
  deriving (Show, Eq)

instance Default HistogramPlot where
  def = HistogramPlot [] Nothing (Just True)

instance ToJSON HistogramPlot where
  toJSON (HistogramPlot vs bs z) =
    omitNulls [ "values" .= vs
              , "bins"   .= bs
              , "zoom"   .= z
              ]

instance ValidatablePlot HistogramPlot where
  validatePlot (HistogramPlot v b z) = do
    b' <- validateBin b
    return $ HistogramPlot v b' z

-- | Submits a request to the specified lightning-viz server to create a plot
-- to histogram.
--
-- <http://lightning-viz.org/visualizations/histogram/ HistogramPlot Visualization>
histogramPlot :: Monad m => HistogramPlot
                            -- ^ Histogram plot to create.
                         -> LightningT m Visualization
                            -- ^ Transformer stack with created visualization.
histogramPlot histPlt = do
  url <- ask
  viz <- sendPlot "histogram" histPlt R.plot
  return $ viz { vizBaseUrl = Just url }
