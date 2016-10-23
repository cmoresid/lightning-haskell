{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a bundled node-link graph.
module Web.Lightning.Plots.GraphBundled
  (
    GraphPlot(..)
  , Visualization(..)
  , graphBundledPlot
  )
  where

--------------------------------------------------------------------------------
import           Control.Monad.Reader

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Plots.Graph         (GraphPlot(..))
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
--------------------------------------------------------------------------------

-- | Submits a request to the specified lightning-viz server to create a
-- bundled node-link graph visualization.
graphBundledPlot :: Monad m => GraphPlot
                               -- ^ Graph plot to create.
                            -> LightningT m Visualization
                               -- ^ Transformer stack with created visualization.
graphBundledPlot graphPlt = do
  url <- ask
  viz <- sendPlot "graph-bundled" graphPlt R.plot
  return $ viz { vizBaseUrl = Just url }
