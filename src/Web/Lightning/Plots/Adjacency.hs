{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Adjacency
  ( AdjacencyPlot(..)
  , Visualization (..)
  , adjacencyPlot
  , defAdjacencyPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data AdjacencyPlot =
  AdjacencyPlot { apConn   :: Maybe [[Double]]
                , apLabel  :: Maybe [Int]
                , apLabels :: Maybe [T.Text] }
  deriving (Show, Eq)

instance Default AdjacencyPlot where
  def = AdjacencyPlot Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''AdjacencyPlot)

defAdjacencyPlot :: AdjacencyPlot
defAdjacencyPlot = def :: AdjacencyPlot

adjacencyPlot :: Monad m => AdjacencyPlot -> LightningT m Visualization
adjacencyPlot adjPlt = sendPlot "adjacency" adjPlt R.plot
