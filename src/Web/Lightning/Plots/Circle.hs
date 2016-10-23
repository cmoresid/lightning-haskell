{-# LANGUAGE OverloadedStrings #-}

-- | Visualize spatial points as a scatter plot.
module Web.Lightning.Plots.Circle
  ( CirclePlot(..)
  , Visualization (..)
  , circlePlot
  ) where

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

-- | Circle plot parameters
data CirclePlot =
  CirclePlot {
                cpConn   :: [[Double]]
                -- ^ Matrix that defines the connectivity of the plot. The
                -- dimensions of the matrix can be (n, n), (n, 2) or (n, 3).
                -- Matrix can be binary or continuous valued. Links should
                -- contain either 2 elements per link (source, target) or
                -- 3 elements (source, target, value).
              , cpGroup  :: Maybe [Int]
                  -- ^ List to set colors via groups.
              , cpColor  :: Maybe [Int]
                -- ^ (k, 3) : List of rbg values to set colors of top-level
                -- group, where k is the number of unique elements in the
                -- top-level group.
              , cpLabels :: Maybe [T.Text]
                -- ^ List of text labels to label nodes.
              }
  deriving (Show, Eq)

instance Default CirclePlot where
  def = CirclePlot [[]] Nothing Nothing Nothing

instance ToJSON CirclePlot where
  toJSON (CirclePlot conn gs cs ls) =
    omitNulls [ "links"  .= getLinks conn
              , "nodes"  .= getNodes conn
              , "group"  .= gs
              , "color"  .= cs
              , "labels" .= ls
              ]

instance ValidatablePlot CirclePlot where
  validatePlot (CirclePlot conn grp cs lbl) = do
    conn' <- validateConn conn
    cs' <- validateColor cs
    return $ CirclePlot conn' grp cs' lbl

-- | Submits a request to the specified lightning-viz server to create
-- a scatter plot.
--
-- <http://lightning-viz.org/visualizations/circle/ Circle Visualization>
circlePlot :: Monad m => CirclePlot
                        -- ^ Circle plot to create.
                      -> LightningT m Visualization
                        -- ^ Transformer stack with created visualization.
circlePlot circlePlt = do
  url <- ask
  viz <- sendPlot "circle" circlePlt R.plot
  return $ viz { vizBaseUrl = Just url }
