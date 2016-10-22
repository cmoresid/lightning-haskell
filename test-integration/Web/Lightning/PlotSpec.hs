{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.PlotSpec where

import           ConfigLightning

import           Control.Monad.IO.Class

import           Test.Hspec

import           Data.Maybe
import qualified Data.Text as T

import           System.Environment

import           Web.Lightning
import           Web.Lightning.Plots
import           Web.Lightning.Session

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  lightning <- runIO configLightning
  urlStr <- runIO $ getEnv "LIGHTNING_VIZ_URL"

  let url = T.pack urlStr

  describe "Lightning Integration Tests" $ do
    it "create Adjacency plot." $ do
      res <- run lightning $ adjacencyPlot url def { apConn = [[1,2,3],[4,5,6],[7,8,9]] }
      res `shouldSatisfy` isRight
    it "create Circle plot." $ do
      res <- run lightning $ circlePlot url def { cpConn = [[1,2,3],[4,5,6],[7,8,9]] }
      res `shouldSatisfy` isRight
    it "create Force plot." $ do
      res <- run lightning $ forcePlot url def { fpConn = [[1,2,3],[4,5,6],[7,8,9]] }
      res `shouldSatisfy` isRight
    it "create Graph plot." $ do
      res <- run lightning $ graphPlot url def { gpX = [1,2,3], gpY = [1,2,3], gpConn = [[1,2,3],[4,5,6],[7,8,9]] }
      res `shouldSatisfy` isRight
    it "create GraphBundled plot." $ do
      res <- run lightning $ graphPlot url def { gpX = [1,2,3], gpY = [1,2,3], gpConn = [[1,2,3],[4,5,6],[7,8,9]] }
      res `shouldSatisfy` isRight
    it "create Histogram plot." $ do
      res <- run lightning $ histogramPlot url def { hpValues = [1,2,2,3,3,3,4,4,5] }
      res `shouldSatisfy` isRight
    it "create Line plot." $ do
      res <- run lightning $ linePlot url def { lpSeries = [[1,2,3]] }
      res `shouldSatisfy` isRight
    it "create Map plot." $ do
      res <- run lightning $ mapPlot url def { mppRegions = ["CAN","USA","MEX"], mppWeights = [13.0,10.0,4.0] }
      res `shouldSatisfy` isRight
    it "create Matrix plot." $ do
      res <- run lightning $ matrixPlot url def { mpMatrix = [[1,1,0],[2,0,0],[3,0,0]] }
      res `shouldSatisfy` isRight
    it "create Scatter plot." $ do
      res <- run lightning $ scatterPlot url def { spX = [1,2,3], spY = [1,1,1] }
      res `shouldSatisfy` isRight
    it "create 3D Scatter plot." $ do
      res <- run lightning $ scatter3Plot url def { sptX = [1,2,3], sptY = [1,1,1], sptZ = [0,0,0] }
      res `shouldSatisfy` isRight
    it "create Streaming line plot." $ do
      res <- run lightning $ streamingLinePlot url def { lspSeries = [[1,2,3]] }
      res `shouldSatisfy` isRight
    it "create Streaming scatter plot." $ do
      res <- run lightning $ streamingScatterPlot url def { sspX = [1,2,3], sspY = [1,2,3] }
      res `shouldSatisfy` isRight
  
