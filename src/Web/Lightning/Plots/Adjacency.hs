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
import           Web.Lightning.Utilities

data AdjacencyPlot =
  AdjacencyPlot { apConn   :: [[Double]]
                , apLabels :: [T.Text]
                , apLabel  :: Maybe [Int] }
  deriving (Show, Eq)

instance Default AdjacencyPlot where
  def = AdjacencyPlot [[]] [] Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''AdjacencyPlot)

defAdjacencyPlot :: AdjacencyPlot
defAdjacencyPlot = def :: AdjacencyPlot

adjacencyPlot :: Monad m => AdjacencyPlot -> LightningT m Visualization
adjacencyPlot adjPlt = sendPlot "adjacency" (transformData adjPlt) R.plot

transformData :: AdjacencyPlot -> Value
transformData (AdjacencyPlot conn ls l) =
  omitNulls [ "links"  .= getLinks conn
            , "nodes"  .= getNodes conn
            , "labels" .= ls
            , "label"  .= l ]
