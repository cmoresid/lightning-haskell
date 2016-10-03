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
import           Web.Lightning.Utilities           (omitNulls, getPoints)

data ScatterPlot =
  ScatterPlot { spX        :: [Double]
              , spY        :: [Double]
              , spLabel    :: Maybe [Int]
              , spGroup    :: Maybe [Int]
              , spSize     :: Maybe [Double]
              , spAlpha    :: Maybe [Double]
              , spXaxis    :: Maybe T.Text
              , spYaxis    :: Maybe T.Text }
  deriving (Show, Eq)

instance Default ScatterPlot where
  def = ScatterPlot [] [] Nothing
    Nothing Nothing Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''ScatterPlot)

defScatterPlot :: ScatterPlot
defScatterPlot = def :: ScatterPlot

scatterPlot :: Monad m => ScatterPlot -> LightningT m Visualization
scatterPlot scatterPlt = sendPlot "scatter" (transformData scatterPlt) R.plot

transformData :: ScatterPlot -> Value
transformData (ScatterPlot xs ys l g s a xl yl) =
  omitNulls [ "points" .= getPoints xs ys
            , "label" .= l
            , "group" .= g
            , "size" .= s
            , "alpha" .= a
            , "xaxis" .= xl
            , "yaxis" .= yl ]
