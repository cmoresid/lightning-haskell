{-# LANGUAGE OverloadedStrings #-}

-- | Visualize one-dimensional series data as lines.
module Web.Lightning.Plots.Line
  (
    LinePlot(..)
  , Visualization (..)
  , linePlot
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities
--------------------------------------------------------------------------------

-- | Line plot parameters
data LinePlot =
  LinePlot { lpSeries    :: [[Double]]
             -- ^ Input data for line plot, typically n series of lenght m. Can
             -- also pass a list where each individual series is of different
             -- lengths.
           , lpIndex     :: Maybe [Int]
             -- ^ Specify the index for the x-axis line plot.
           , lpColor     :: Maybe [Int]
             -- ^ Single RGB value or list to set line colors to.
           , lpGroup     :: Maybe [Int]
             -- ^ Single integer or list to set line colors to via group
             -- assignment.
           , lpThickness :: Maybe [Int]
             -- ^ Single integer or list to set line thickness to.
           , lpXaxis     :: Maybe T.Text
             -- ^ Label for x-axis.
           , lpYaxis     :: Maybe T.Text
             -- ^ Label for y-axis.
           , lpZoom      :: Maybe Bool
             -- ^ Whether or not to allow zooming.
           }
  deriving (Show, Eq)

instance Default LinePlot where
  def = LinePlot [[]] Nothing Nothing Nothing Nothing Nothing Nothing (Just True)

instance ToJSON LinePlot where
  toJSON (LinePlot ss is cs gs t xa ya z) =
    omitNulls [ "series"    .= ss
              , "index"     .= is
              , "color"     .= cs
              , "group"     .= gs
              , "thickness" .= t
              , "xaxis"     .= xa
              , "yaxis"     .= ya
              , "zoom"      .= z
              ]

instance ValidatablePlot LinePlot where
  validatePlot = return

-- | Submits a request to the specified lightning-viz server to create a plot
-- to visualize one-dimensional series.
--
-- <http://lightning-viz.org/visualizations/line/ Line Visualization>
linePlot :: Monad m => T.Text
                       -- ^ Base URL for lightning-viz server.
                    -> LinePlot
                       -- ^ Line plot to create.
                    -> LightningT m Visualization
                       -- ^ Transformer stack with created visualization.
linePlot bUrl linePlt = do
  viz <- sendPlot "line" linePlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
