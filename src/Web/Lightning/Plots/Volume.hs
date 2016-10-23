{-# LANGUAGE OverloadedStrings #-}

-- | Visualize a collection of images as a three-dimensional volume.
module Web.Lightning.Plots.Volume
  (
    VolumePlot(..)
  , Visualization (..)
  , volumePlot
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning
import           Web.Lightning.Types               (Img)
import           Web.Lightning.Types.Visualization (Visualization (..))
--------------------------------------------------------------------------------

-- | Volume plot parameters
data VolumePlot = VolumePlot { vpImages  ::  [Img]
                               -- ^ A collection of images to display. Can be
                               -- 2 dimensional (grey scale) or 3 dimensional
                               -- (RGB) lists.
                             }
                             deriving (Show, Eq)

instance Default VolumePlot where
  def = VolumePlot [ [[[]]] ]

instance ToJSON VolumePlot where
  toJSON (VolumePlot imgs) =
    object [ "images"  .= imgs ]

instance ValidatablePlot VolumePlot where
  validatePlot = return

-- | Submits a request to the specified lightning-viz server to create
-- a visualization of a collection of images as a three-dimensional volume.
--
-- <http://lightning-viz.org/visualizations/volume/ Volume Visualization>
volumePlot :: Monad m => T.Text
                      -- ^ Base URL for lightning-viz server.
                      -> VolumePlot
                      -- ^ Volume plot to create
                      -> LightningT m Visualization
                      -- ^ Transformer stack with created visualization.
volumePlot bUrl volumePlt = do
  viz <- sendPlot "volume" volumePlt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
