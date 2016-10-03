{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Lightning.Plots.Matrix
  ( MatrixPlot(..)
  , Visualization (..)
  , matrixPlot
  , defMatrixPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))

data MatrixPlot =
  MatrixPlot { mpMatrix    :: Maybe [[Double]]
             , mpColorMap  :: Maybe T.Text
             , mpRowLabels :: Maybe [T.Text]
             , mpColLabels :: Maybe [T.Text] }
  deriving (Show, Eq)

instance Default MatrixPlot where
  def = MatrixPlot Nothing Nothing Nothing Nothing

$(deriveToJSON defaultOptions { omitNothingFields = True} ''MatrixPlot)

defMatrixPlot :: MatrixPlot
defMatrixPlot = def :: MatrixPlot

matrixPlot :: Monad m => MatrixPlot -> LightningT m Visualization
matrixPlot matrixPlt = sendPlot "matrix" matrixPlt R.plot
