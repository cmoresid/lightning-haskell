{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Plots.Matrix
  ( MatrixPlot(..)
  , Visualization (..)
  , matrixPlot
  , defMatrixPlot
  , module Data.Default.Class
  ) where

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text                         as T

import qualified Web.Lightning.Routes              as R
import           Web.Lightning.Types.Lightning     (LightningT, sendPlot)
import           Web.Lightning.Types.Visualization (Visualization (..))
import           Web.Lightning.Utilities

data MatrixPlot =
  MatrixPlot { mpMatrix    :: Maybe [[Double]]
             , mpColorMap  :: Maybe T.Text
             , mpRowLabels :: Maybe [T.Text]
             , mpColLabels :: Maybe [T.Text] }
  deriving (Show, Eq)

instance Default MatrixPlot where
  def = MatrixPlot Nothing Nothing Nothing Nothing

instance ToJSON MatrixPlot where
  toJSON mp = omitNulls
    [
      "matrix" .= mpMatrix mp
    , "colormap" .= mpColorMap mp
    , "rowLabels" .= mpRowLabels mp
    , "colLabels" .= mpColLabels mp
    ]

defMatrixPlot :: MatrixPlot
defMatrixPlot = def :: MatrixPlot

matrixPlot :: Monad m => MatrixPlot -> LightningT m Visualization
matrixPlot matrixPlt = sendPlot "matrix" matrixPlt R.plot
