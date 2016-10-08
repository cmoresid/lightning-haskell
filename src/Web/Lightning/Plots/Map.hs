{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Plots.Map
  ( MapPlot(..)
  , Visualization (..)
  , mapPlot
  , defMapPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities

data MapPlot =
  MapPlot { mppRegions  :: Maybe [T.Text]
          , mppValues   :: Maybe [Double]
          , mppColorMap :: Maybe T.Text }
  deriving (Show, Eq)

instance Default MapPlot where
  def = MapPlot Nothing Nothing Nothing

instance ToJSON MapPlot where
  toJSON mp = omitNulls
    [
      "regions" .= mppRegions mp
    , "values" .= mppValues mp
    , "colormap" .= mppColorMap mp
    ]

defMapPlot :: MapPlot
defMapPlot = def :: MapPlot

mapPlot :: Monad m => MapPlot -> LightningT m Visualization
mapPlot mapPlt = sendPlot "map" mapPlt R.plot
