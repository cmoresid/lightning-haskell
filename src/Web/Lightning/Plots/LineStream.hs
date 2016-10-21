{-# LANGUAGE OverloadedStrings #-}

-- | Visualize one-dimensional series data as lines.
module Web.Lightning.Plots.LineStream
  (
    LineStreamPlot(..)
  , Visualization (..)
  , streamingLinePlot
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

-- | Streaming line plot parameters
data LineStreamPlot =
  LineStreamPlot { lspSeries    :: [[Double]]
                   -- ^ Input data for line plot, typically n series of lenght m. Can
                   -- also pass a list where each individual series is of different
                   -- lengths.
                 , lspIndex     :: Maybe [Int]
                   -- ^ Specify the index for the x-axis line plot.
                 , lspColor     :: Maybe [Int]
                   -- ^ Single RGB value or list to set line colors to.
                 , lspGroup     :: Maybe [Int]
                   -- ^ Single integer or list to set line colors to via group
                   -- assignment.
                 , lspSize      :: Maybe [Int]
                   -- ^ Sets the line thickness
                 , lspXAxis     :: Maybe T.Text
                   -- ^ Label for the x axis
                 , lspYAxis     :: Maybe T.Text
                   -- ^ Label for the y axis
                 , lspMaxWidth  :: Maybe Int
                   -- ^ The maximum number of time points to show before plot shifts.
                 , lspZoom      :: Maybe Bool
                 }
  deriving (Show, Eq)

instance Default LineStreamPlot where
  def = LineStreamPlot [[]] Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing (Just True)

instance ToJSON LineStreamPlot where
  toJSON (LineStreamPlot ss is cs gs t xa ya mw z) =
    omitNulls [ "series"    .= ss
              , "index"     .= is
              , "color"     .= cs
              , "group"     .= gs
              , "size"      .= t
              , "xaxis"     .= xa
              , "yaxis"     .= ya
              , "max_width" .= mw
              , "zoom"      .= z
              ]

instance ValidatablePlot LineStreamPlot where
  validatePlot (LineStreamPlot ss i c g s xa ya mw z) = do
    i' <- validateIndex i
    c' <- validateColor c
    s' <- validateSize s
    return $ LineStreamPlot ss i' c' g s' xa ya mw z

-- | Plot streaming one-dimensional series data as updating lines.
--
-- <http://lightning-viz.org/visualizations/streaming/ Streaming Line Visualization>
streamingLinePlot :: Monad m => T.Text
                       -- ^ Base URL for lightning-viz server.
                    -> Maybe Visualization
                       -- ^ The visualization to update. If Nothing, create a
                       -- new plot.
                    -> LineStreamPlot
                       -- ^ Line plot to create / update.
                    -> LightningT m Visualization
                       -- ^ Transformer stack with created visualization.
streamingLinePlot bUrl viz slp = do
  viz' <- streamPlot viz "line-streaming" slp R.plot
  return $ viz' { vizBaseUrl = Just bUrl }
