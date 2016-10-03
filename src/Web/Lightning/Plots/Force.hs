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
import           Web.Lightning.Utilities

data ForcePlot =
  ForcePlot { fpConn     :: [[Double]]
            , fpGroup    :: [Int]
            , fpLabel    :: [T.Text] }
  deriving (Show, Eq)

instance Default ForcePlot where
  def = ForcePlot [[]] [] []

$(deriveToJSON defaultOptions { omitNothingFields = True} ''ForcePlot)

defForcePlot :: ForcePlot
defForcePlot = def :: ForcePlot

forcePlot :: Monad m => ForcePlot -> LightningT m Visualization
forcePlot forcePlt = sendPlot "force" (transformData forcePlt) R.plot

transformData :: ForcePlot -> Value
transformData (ForcePlot conn g ls) =
  omitNulls [ "links"  .= getLinks conn
            , "nodes"  .= getNodes conn
            , "labels" .= ls
            , "group"  .= g ]
