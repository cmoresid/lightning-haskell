{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Scatter
  ( ScatterPlot(..)
  , Visualization (..)
  , scatterPlot
  , defScatterPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data ScatterPlot =
  ScatterPlot { spX        :: Maybe [Double]
              , spY        :: Maybe [Double]
              , spLabel    :: Maybe [Int]
              , spValue    :: Maybe [Double]
              , spGroup    :: Maybe [Int]
              , spColorMap :: Maybe T.Text
              , spSize     :: Maybe [Double]
              , spAlpha    :: Maybe [Double]
              , spXaxis    :: Maybe T.Text
              , spYaxis    :: Maybe T.Text }
  deriving (Show, Eq)

instance Default ScatterPlot where
  def = ScatterPlot Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing 

$(deriveToJSON defaultOptions { omitNothingFields = True} ''ScatterPlot)

defScatterPlot :: ScatterPlot
defScatterPlot = def :: ScatterPlot

scatterPlot :: Monad m => ScatterPlot -> LightningT m Visualization
scatterPlot scatterPlt = sendPlot "scatter" scatterPlt R.plot
