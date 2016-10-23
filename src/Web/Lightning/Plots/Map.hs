{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a chlorpleth map of the world or united states.
module Web.Lightning.Plots.Map
  (
    MapPlot(..)
  , Visualization (..)
  , mapPlot
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

-- | Map plot parameters
data MapPlot =
  MapPlot { mppRegions  ::  [T.Text]
            -- ^ String identifies for map regions, either length of two
            -- characters (for states in a US map) or length of three
            -- (for counties in a world map).
          , mppWeights  :: [Double]
            -- ^ Values to use to color each reason
          , mppColorMap :: Maybe T.Text
            -- ^ Specification of color map; only colorbrew types supported.
          }
  deriving (Show, Eq)

instance Default MapPlot where
  def = MapPlot [] [] Nothing

instance ToJSON MapPlot where
  toJSON (MapPlot rs vs cm) =
    omitNulls [ "regions"  .= rs
              , "values"   .= vs
              , "colormap" .= cm
              ]

instance ValidatablePlot MapPlot where
  validatePlot (MapPlot r v cm) = do
    r' <- validateRegion r
    cm' <- validateColorMap cm
    return $ MapPlot r' v cm'

-- | Submits a request to the specified lightning-viz server to create a
-- chloropleth map of the world or united states.
--
-- <http://lightning-viz.org/visualizations/map/ Map Visualization>
mapPlot :: Monad m => MapPlot
                      -- ^ Map plot to create.
                   -> LightningT m Visualization
                      -- ^ Transformer stack with created visualization.
mapPlot mapPlt = do
  url <- ask
  viz <- sendPlot "map" mapPlt R.plot
  return $ viz { vizBaseUrl = Just url }
