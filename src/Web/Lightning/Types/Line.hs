{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.Line
  ( LinePlot(..)
  ) where

import           Data.Aeson
import           Data.Default.Class
import qualified Data.Text          as T
import Web.Lightning.Utilities

data LinePlot =
  LinePlot { series :: [[Double]]
           , index :: Maybe [Int]
           , thickness :: Maybe [Int]
           , xaxis :: Maybe T.Text
           , yaxis :: Maybe T.Text }
  deriving (Show, Eq)

instance Default LinePlot where
  def = LinePlot [[]] Nothing Nothing Nothing Nothing

instance ToJSON LinePlot where
  toJSON linePlot = omitNulls
    [
      "series" .= series linePlot
    , "index" .= index linePlot
    , "thickness" .= thickness linePlot
    , "xaxis" .= xaxis linePlot
    , "yaxis" .= yaxis linePlot
    ]
