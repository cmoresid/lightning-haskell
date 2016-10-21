{-# LANGUAGE OverloadedStrings #-}

-- | Visualize one-dimensional series data as lines.
module Web.Lightning.Plots.ScatterStream
  (
    ScatterStreamPlot(..)
  , Visualization (..)
  , streamingScatterPlot
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

-- | Scatter plot parameters
data ScatterStreamPlot =
  ScatterStreamPlot { sspX        :: [Double]
                      -- ^ List of x points.
                    , sspY        :: [Double]
                      -- ^ List of y points.
                    , sspValues   :: Maybe [Double]
                      -- ^ Values to set node colors via a linear scale.
                    , sspLabels   :: Maybe [T.Text]
                      -- ^ List of text labels to set tooltips.
                    , sspColor    :: Maybe [Int]
                      -- ^ List of rgb values to set colors.
                    , sspGroup    :: Maybe [Int]
                      -- ^ List to set colors via groups.
                    , sspColorMap :: Maybe T.Text
                      -- ^ Specification of color map; only colorbrewer types supported.
                    , sspSize     :: Maybe [Int]
                      -- ^ List to set point sizes.
                    , sspXaxis    :: Maybe T.Text
                      -- ^ Label for x-axis.
                    , sspYaxis    :: Maybe T.Text
                      -- ^ Label for y-axis.
                    , sspToolTips :: Maybe Bool
                      -- ^ Whether or not to display tooltips.
                    , sspZoom     :: Maybe Bool
                      -- ^ Whether or not to allow zooming.
                    , sspBrush    :: Maybe Bool
                      -- ^ Whether or not to support brushing.
                    }
  deriving (Show, Eq)

instance Default ScatterStreamPlot where
  def = ScatterStreamPlot [] [] Nothing Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing (Just True) (Just True) (Just True)

instance ToJSON ScatterStreamPlot where
  toJSON (ScatterStreamPlot xs ys vs ls cs gs cm ss xa ya t z b) =
    omitNulls [ "points"    .= getPoints xs ys
              , "values"    .= vs
              , "labels"    .= ls
              , "color"     .= cs
              , "group"     .= gs
              , "colormap"  .= cm
              , "size"      .= ss
              , "xaxis"     .= xa
              , "yaxis"     .= ya
              , "tooltips"  .= t
              , "zoom"      .= z
              , "brush"     .= b
              ]

instance ValidatablePlot ScatterStreamPlot where
  validatePlot (ScatterStreamPlot xs ys v lbl c grp cm s xa ya tt z b) = do
    (xs', ys') <- validateCoordinates xs ys
    c' <- validateColor c
    cm' <- validateColorMap cm
    s' <- validateSize s
    return $ ScatterStreamPlot xs' ys' v lbl c' grp cm' s' xa ya tt z b

-- | Create a streaming scatter plot of x and y.
--
-- Plotting once returns a visualization on which 'append' can be called to add new data
-- in a streaming fashion. The opacity of old and new data is automatically set
-- to highlight the most recent data and fade old data away.
--
-- <http://lightning-viz.org/visualizations/streaming/ Streaming Scatter Visualization>
streamingScatterPlot :: Monad m => T.Text
                       -- ^ Base URL for lightning-viz server.
                    -> Maybe Visualization
                       -- ^ The visualization to update. If Nothing, create a
                       -- new plot.
                    -> ScatterStreamPlot
                       -- ^ Scatter plot to create / update.
                    -> LightningT m Visualization
                       -- ^ Transformer stack with created visualization.
streamingScatterPlot bUrl viz slp = do
  viz' <- streamPlot viz "scatter-streaming" slp R.plot
  return $ viz' { vizBaseUrl = Just bUrl }
