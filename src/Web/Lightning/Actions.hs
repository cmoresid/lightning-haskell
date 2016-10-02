{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Actions where

import qualified Web.Lightning.Routes as R
import Web.Lightning.Types
import Web.Lightning.Types.Lightning
import Web.Lightning.Types.Visualization

linePlot :: Monad m => LinePlot -> LightningT m Visualization
linePlot lp = sendPlot "line" lp R.plot
