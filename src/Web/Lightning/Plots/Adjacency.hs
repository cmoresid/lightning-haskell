{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a sparse adjacency matrix.
module Web.Lightning.Plots.Adjacency
  (
    AdjacencyPlot(..)
  , Visualization (..)
  , adjacencyPlot
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

-- | Adjacency plot parameters
data AdjacencyPlot =
  AdjacencyPlot { apConn   :: [[Double]]
                  -- ^ Matrix that defines the connectivity of the plot. The
                  -- dimensions of the matrix can be (n, n), (n, 2) or (n, 3).
                  -- Matrix can be binary or continuous valued. Links should
                  -- contain either 2 elements per link (source, target) or
                  -- 3 elements (source, target, value).
                , apLabels :: Maybe [T.Text]
                  -- ^ Text labels for each item (will label rows and columns).
                , apGroup  :: Maybe [Int]
                  -- ^ List to set colors via groups.
                , apSort   :: Maybe T.Text
                  -- ^ What to sort by; options are "group" or "degree."
                , apNumbers :: Maybe Bool
                  -- ^ Whether or not to show numbers on cells.
                , apSymmetric :: Maybe Bool
                  -- ^ Whether or not to make links symetrical.
                }
  deriving (Show, Eq)

instance Default AdjacencyPlot where
  def = AdjacencyPlot [[]] Nothing Nothing (Just "group") (Just False) (Just True)

instance ToJSON AdjacencyPlot where
  toJSON (AdjacencyPlot conn lbs grps srt nbrs sym) =
    omitNulls [ "links"     .= getLinks conn
              , "nodes"     .= getNodes conn
              , "labels"    .= lbs
              , "group"     .= grps
              , "numbers"   .= nbrs
              , "symmetric" .= sym
              , "srt"       .= srt
              ]

instance ValidatablePlot AdjacencyPlot where
  validatePlot = return

-- | Submits a request to the specified lightning-viz server to create
-- a sparse adjacency matrix visualiazation.
--
-- <http://lightning-viz.org/visualizations/adjacency/ Adjacency Visualization>
adjacencyPlot :: Monad m => T.Text
                            -- ^ Base URL for lightning-viz server.
                         -> AdjacencyPlot
                            -- ^ Adjacency plot to create.
                         -> LightningT m Visualization
                            -- ^ Transformer stack with created visualization.
adjacencyPlot bUrl adjPlt = do
  viz <- sendPlot "adjacency" adjPlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
