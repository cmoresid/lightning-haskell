{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.Line
  ( LinePlot(..)
  ) where

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text          as T

data LinePlot =
  LinePlot { series :: [[Double]]
           , label  :: [Int]
           , size   :: [Double]
           , alpha  :: [Double]
           , xaxis  :: T.Text
           , yaxis  :: T.Text }
  deriving (Show, Eq)

instance Default LinePlot where
  def = LinePlot [[]] [] [] [] "" ""

instance ToJSON LinePlot where
  toJSON linePlot = object
    [
      "series" .= series linePlot
    , "label" .= label linePlot
    , "size" .= size linePlot
    , "alpha" .= alpha linePlot
    , "xaxis" .= xaxis linePlot
    , "yaxis" .= yaxis linePlot
    ]
