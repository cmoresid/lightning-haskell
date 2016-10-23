{-# LANGUAGE OverloadedStrings #-}

-- | Visualize spatial points as a scatter plot.
module Web.Lightning.Plots.Scatter
  (
    ScatterPlot(..)
  , Visualization (..)
  , scatterPlot
  )
  where

--------------------------------------------------------------------------------
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities
--------------------------------------------------------------------------------

-- | Scatter plot parameters
data ScatterPlot =
  ScatterPlot { spX        :: [Double]
                -- ^ List of x points.
              , spY        :: [Double]
                -- ^ List of y points.
              , spValues   :: Maybe [Double]
                -- ^ Values to set node colors via a linear scale.
              , spLabels   :: Maybe [T.Text]
                -- ^ List of text labels to set tooltips.
              , spColor    :: Maybe [Int]
                -- ^ List of rgb values to set colors.
              , spGroup    :: Maybe [Int]
                -- ^ List to set colors via groups.
              , spColorMap :: Maybe T.Text
                -- ^ Specification of color map; only colorbrewer types supported.
              , spSize     :: Maybe [Int]
                -- ^ List to set point sizes.
              , spAlpha    :: Maybe [Double]
                -- ^ List of alpha values to set file and stroke opacity.
              , spXaxis    :: Maybe T.Text
                -- ^ Label for x-axis.
              , spYaxis    :: Maybe T.Text
                -- ^ Label for y-axis.
              , spToolTips :: Maybe Bool
                -- ^ Whether or not to display tooltips.
              , spZoom     :: Maybe Bool
                -- ^ Whether or not to allow zooming.
              , spBrush    :: Maybe Bool
                -- ^ Whether or not to support brushing.
              }
  deriving (Show, Eq)

instance Default ScatterPlot where
  def = ScatterPlot [] [] Nothing Nothing Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing (Just True) (Just True) (Just True)

instance ToJSON ScatterPlot where
  toJSON (ScatterPlot xs ys vs ls cs gs cm ss as xa ya t z b) =
    omitNulls [ "points"    .= getPoints xs ys
              , "values"    .= vs
              , "labels"    .= ls
              , "color"     .= cs
              , "group"     .= gs
              , "colormap"  .= cm
              , "size"      .= ss
              , "alpha"     .= as
              , "xaxis"     .= xa
              , "yaxis"     .= ya
              , "tooltips"  .= t
              , "zoom"      .= z
              , "brush"     .= b
              ]

instance ValidatablePlot ScatterPlot where
  validatePlot (ScatterPlot xs ys v lbl c grp cm s a xa ya tt z b) = do
    (xs', ys') <- validateCoordinates xs ys
    c' <- validateColor c
    cm' <- validateColorMap cm
    s' <- validateSize s
    a' <- validateAlpha a
    return $ ScatterPlot xs' ys' v lbl c' grp cm' s' a' xa ya tt z b

-- | Submits a request to the specified lightning-viz server to create
-- a scatter plot.
--
-- <http://lightning-viz.org/visualizations/adjacency/ Scatter Visualization>
scatterPlot :: Monad m => ScatterPlot
                          -- ^ Scatter plot to create.
                       -> LightningT m Visualization
                          -- ^ Transformer stack with created visualization.
scatterPlot scatterPlt = do
  url <- ask
  viz <- sendPlot "scatter" scatterPlt R.plot
  return $ viz { vizBaseUrl = Just url }
