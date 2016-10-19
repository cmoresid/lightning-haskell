{-# LANGUAGE OverloadedStrings #-}

-- | Visualize graph structure.
module Web.Lightning.Plots.Graph
  (
    GraphPlot(..)
  , Visualization (..)
  , graphPlot
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

-- | Graph plot parameters
data GraphPlot =
  GraphPlot { gpX        :: [Double]
              -- ^ x-points for node co-ordinates.
            , gpY        :: [Double]
              -- ^ y-points for node co-ordinates.
            , gpConn     :: [[Double]]
              -- ^ Matrix that defines the connectivity of the plot. The
              -- dimensions of the matrix can be (n, n), (n, 2) or (n, 3).
              -- Matrix can be binary or continuous valued. Links should
              -- contain either 2 elements per link (source, target) or
              -- 3 elements (source, target, value).
            , gpValues   :: Maybe [Int]
              -- ^ Values to set node colors via a linear scale.
            , gpLabels   :: Maybe [T.Text]
              -- ^ List of text labels to be used as tooltips.
            , gpColors   :: Maybe [Int]
              -- ^ List to set node colors.
            , gpGroup    :: Maybe [Int]
              -- ^ List to set node colors via group assignment.
            , gpColorMap :: Maybe T.Text
              -- ^ Specification of color map; only colorbrewer types supported.
            , gpSize     :: Maybe [Int]
              -- ^ List to set node sizes.
            , gpToolTips :: Maybe Bool
              -- ^ Whether or not to show tooltips.
            , gpZoom     :: Maybe Bool
              -- ^ Whether or not to allow zooming.
            , gpBrush    :: Maybe Bool
              -- ^ Whether or not to support brushing.
            }
  deriving (Show, Eq)

instance Default GraphPlot where
  def = GraphPlot [] [] [[]] Nothing Nothing Nothing Nothing Nothing
          Nothing (Just True) (Just True) (Just True)

instance ToJSON GraphPlot where
  toJSON (GraphPlot xs ys conn vs ls cs gs cm ss t z b) =
    omitNulls [ "links" .= getLinks conn
              , "nodes" .= getPoints xs ys
              , "values" .= vs
              , "labels" .= ls
              , "color"  .= cs
              , "group" .= gs
              , "colormap" .= cm
              , "size"  .= ss
              , "tooltips" .= t
              , "zoom" .= z
              , "brush" .= b
              ]

instance ValidatablePlot GraphPlot where
  validatePlot (GraphPlot xs ys conn vl lbl c g cm s tt z b) = do
    (xs', ys') <- validateCoordinates xs ys
    conn' <- validateConn conn
    c' <- validateColor c
    cm' <- validateColorMap cm
    s' <- validateSize s
    return $ GraphPlot xs' ys' conn' vl lbl c' g cm' s' tt z b

-- | Submits a request to the specified lightning-viz server to create a
-- node-link graph from spatial points and their connectivity.
graphPlot :: Monad m => T.Text
                        -- ^ Base URL for lightning-viz server.
                     -> GraphPlot
                        -- ^ Graph plot to create.
                     -> LightningT m Visualization
                        -- ^ Transformer stack with created visualization.
graphPlot bUrl graphPlt = do
  viz <- sendPlot "graph" graphPlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
