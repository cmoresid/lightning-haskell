{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Map
  ( MapPlot(..)
  , Visualization (..)
  , mapPlot
  , defMapPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data MapPlot =
  MapPlot { mppRegions  :: Maybe [T.Text]
          , mppValues   :: Maybe [Double]
          , mppColorMap :: Maybe [Int] }
  deriving (Show, Eq)

instance Default MapPlot where
  def = MapPlot Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''MapPlot)

defMapPlot :: MapPlot
defMapPlot = def :: MapPlot

mapPlot :: Monad m => MapPlot -> LightningT m Visualization
mapPlot mapPlt = sendPlot "map" mapPlt R.plot
