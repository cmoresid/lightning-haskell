{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Scatter3
  ( Scatter3Plot(..)
  , Visualization (..)
  , scatter3Plot
  , defScatter3Plot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities           (omitNulls, getPoints3)

data Scatter3Plot =
  Scatter3Plot { sptX     :: [Double]
               , sptY     :: [Double]
               , sptZ     :: [Double]
               , sptLabel :: Maybe [Int]
               , sptSize  :: Maybe [Double] }
  deriving (Show, Eq)

instance Default Scatter3Plot where
  def = Scatter3Plot [] [] [] Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''Scatter3Plot)

defScatter3Plot :: Scatter3Plot
defScatter3Plot = def :: Scatter3Plot

scatter3Plot :: Monad m => Scatter3Plot -> LightningT m Visualization
scatter3Plot scatter3Plt =
  sendPlot "scatter-3" (transformData scatter3Plt) R.plot

transformData :: Scatter3Plot -> Value
transformData (Scatter3Plot xs ys zs l s) =
  omitNulls [ "points" .= getPoints3 xs ys zs
            , "label" .= l
            , "size" .= s]
