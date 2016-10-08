{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a force-directed network from connectivity.
module Web.Lightning.Plots.Force
  ( ForcePlot(..)
  , Visualization (..)
  , forcePlot
  , module Data.Default.Class
  ) where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities
--------------------------------------------------------------------------------

-- | Force plot parameters
data ForcePlot =
  ForcePlot { fpConn     :: [[Double]]
              -- ^ Matrix that defines the connectivity of the plot. The
              -- dimensions of the matrix can be (n, n), (n, 2) or (n, 3).
              -- Matrix can be binary or continuous valued. Links should
              -- contain either 2 elements per link (source, target) or
              -- 3 elements (source, target, value).
            , fpValues   :: Maybe [Double]
              -- ^ Values to set node colors via a linear scale.
            , fpLabels   :: Maybe [T.Text]
              -- ^ List of text labels to set as tooltips.
            , fpColor    :: Maybe [Int]
              -- ^ Single RGB value or list to set node colors.
            , fpGroup    :: Maybe [Int]
              -- ^ Single integer or list to set node colors via groups.
            , fpColorMap :: Maybe T.Text
              -- ^ Specification of color map, only colorbrewer types supported.
            , fpSize     :: Maybe [Int]
              -- ^ Single size or list to set node sizes.
            , fpToolTips :: Maybe Bool
              -- ^ Whether or not to show tooltips.
            , fpZoom     :: Maybe Bool
              -- ^ Whether or not to allow zooming.
            , fpBrush    :: Maybe Bool
              -- ^ Whether or not to support brushing.
            }
  deriving (Show, Eq)

instance Default ForcePlot where
  def = ForcePlot [[]] Nothing Nothing Nothing Nothing Nothing
          Nothing (Just True) (Just True) (Just True)

instance ToJSON ForcePlot where
  toJSON (ForcePlot conn vs lbs cs gs cm ss tt z b) =
    omitNulls [ "links"     .= getLinks conn
              , "nodes"     .= getNodes conn
              , "values"    .= vs
              , "labels"    .= lbs
              , "color"     .= cs
              , "group"     .= gs
              , "colormap"  .= cm
              , "size"      .= ss
              , "tooltips"  .= tt
              , "zoom"      .= z
              , "brush"     .= b
              ]

-- | Submits a request to the specified lightning-viz server to create a
-- force-directed network visualization from connectivity.
--
-- <http://lightning-viz.org/visualizations/force/ Force-Directed Network Visualization>
forcePlot :: Monad m => T.Text
                        -- ^ Base URL for lightning-viz server.
                     -> ForcePlot
                        -- ^ Force plot to create.
                     -> LightningT m Visualization
                        -- ^ Transformer stack with created visualization.
forcePlot bUrl forcePlt = do
  viz <- sendPlot "force" forcePlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
