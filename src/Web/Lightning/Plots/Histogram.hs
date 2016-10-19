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
import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities           (omitNulls)
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
  validatePlot = return

-- | Submits a request to the specified lightning-viz server to create a plot
-- to histogram.
--
-- <http://lightning-viz.org/visualizations/histogram/ HistogramPlot Visualization>
histogramPlot :: Monad m => T.Text
                            -- ^ Base URL for lightning-viz server.
                         -> HistogramPlot
                            -- ^ Histogram plot to create.
                         -> LightningT m Visualization
                            -- ^ Transformer stack with created visualization.
histogramPlot bUrl histPlt = do
  viz <- sendPlot "histogram" histPlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
