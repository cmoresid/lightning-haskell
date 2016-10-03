{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Force
  ( ForcePlot(..)
  , Visualization (..)
  , forcePlot
  , defForcePlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data ForcePlot =
  ForcePlot { fpConn     :: Maybe [[Double]]
            , fpLabel    :: Maybe [Int]
            , fpValue    :: Maybe [Double]
            , fpColorMap :: Maybe T.Text
            , fpSize     :: Maybe [Double] }
  deriving (Show, Eq)

instance Default ForcePlot where
  def = ForcePlot Nothing Nothing Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''ForcePlot)

defForcePlot :: ForcePlot
defForcePlot = def :: ForcePlot

forcePlot :: Monad m => ForcePlot -> LightningT m Visualization
forcePlot forcePlt = sendPlot "force" forcePlt R.plot
