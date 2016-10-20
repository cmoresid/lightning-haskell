{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a dense matrix or table as a heat map.
module Web.Lightning.Plots.Matrix
  (
    MatrixPlot(..)
  , Visualization (..)
  , matrixPlot
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

-- | Matrix plot parameters
data MatrixPlot =
  MatrixPlot { mpMatrix    :: [[Double]]
               -- ^ Two-dimensional list of matrix data.
             , mpRowLabels :: Maybe [T.Text]
               -- ^ List of strings to label rows.
             , mpColLabels :: Maybe [T.Text]
               -- ^ List of strings to label columns.
             , mpColorMap  :: Maybe T.Text
               -- ^ Specification of color map; only colorbrewer types supported.
             , mpNumbers   :: Maybe Bool
               -- ^ Whether or not to show numbers on the cells.
             }
  deriving (Show, Eq)

instance Default MatrixPlot where
  def = MatrixPlot [[]] Nothing Nothing Nothing (Just False)

instance ToJSON MatrixPlot where
  toJSON (MatrixPlot m rls cls cm nbrs) =
    omitNulls [ "matrix"    .= m
              , "rowLabels" .= rls
              , "colLabels" .= cls
              , "colormap"  .= cm
              , "numbers"   .= nbrs
              ]

instance ValidatablePlot MatrixPlot where
  validatePlot (MatrixPlot mtx rlbl clbl cm nbr) = do
    mtx' <- validateConn mtx
    cm' <- validateColorMap cm
    return $ MatrixPlot mtx' rlbl clbl cm' nbr

-- | Submits a request to the specified lightning-viz server to create a
-- heat map of the given matrix.
--
-- <http://lightning-viz.org/visualizations/matrix/ Matrix Visualization>
matrixPlot :: Monad m => T.Text
                         -- ^ Base URL for lightning-viz server.
                      -> MatrixPlot
                         -- ^ Matrix plot to create.
                      -> LightningT m Visualization
                         -- ^ Transformer stack with created visualization.
matrixPlot bUrl matrixPlt = do
  viz <- sendPlot "matrix" matrixPlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
