{-# LANGUAGE OverloadedStrings #-}

-- | Visualize (x,y,z) co-ordinates as a 3D scatter plot.
module Web.Lightning.Plots.Scatter3
  (
    Scatter3Plot(..)
  , Visualization (..)
  , scatter3Plot
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text                         as T
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities           (omitNulls, getPoints3)
--------------------------------------------------------------------------------

-- | Scatter Plot 3D parameters
data Scatter3Plot =
  Scatter3Plot { sptX      :: [Double]
                 -- ^ List of x points.
               , sptY      :: [Double]
                 -- ^ List of y points.
               , sptZ      :: [Double]
                 -- ^ List of z points.
               , sptColors :: Maybe [Int]
                 -- ^ List of rgb values to set colors.
               , sptGroups :: Maybe [Int]
                 -- ^ List to set colors via groups.
               , sptSize   :: Maybe [Int]
                 -- ^ List to set point sizes.
               , sptAlpha  :: Maybe [Double]
                 -- ^ List of alpha values to set file and stroke opacity.
               }
  deriving (Show, Eq)

instance Default Scatter3Plot where
  def = Scatter3Plot [] [] [] Nothing Nothing Nothing Nothing

instance ToJSON Scatter3Plot where
  toJSON (Scatter3Plot xs ys zs cs gs ss as) =
    omitNulls [ "points"    .= getPoints3 xs ys zs
              , "color"     .= cs
              , "group"     .= gs
              , "size"      .= ss
              , "alpha"     .= as
              ]
-- | Submits a request to the specified lightning-viz server to create
-- a 3D scatter plot.
--
-- <http://lightning-viz.org/visualizations/scatter-3/ Scatter Visualization>
scatter3Plot :: Monad m => T.Text
                           -- ^ Base URL for lightning-viz server.
                        -> Scatter3Plot
                           -- ^ Scatter plot to create
                        -> LightningT m Visualization
                           -- ^ Transformer stack with created visualization.
scatter3Plot bUrl scatter3Plt = do
  viz <- sendPlot "scatter-3" scatter3Plt R.plot
  return $ viz { vizBaseUrl = Just bUrl }
