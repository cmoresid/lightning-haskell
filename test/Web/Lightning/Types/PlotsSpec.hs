{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.PlotsSpec where

import           Test.Hspec

import           Data.Aeson
import           Data.Maybe

import           Web.Lightning.Plots

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lightning Plot Serialization" $ do
    it "encodes default LinePlot property names" $ do
      let testInput = def { lpSeries = [[1,2,3]] } :: LinePlot
        in encode testInput `shouldBe` "{\"series\":[[1,2,3]],\"zoom\":true}"
    it "encodes default HistogramPlot property names" $ do
      let testInput = def { hpValues = [1,1,2,2,2,3,3,3], hpZoom = Just False } :: HistogramPlot
        in encode testInput `shouldBe` "{\"values\":[1,1,2,2,2,3,3,3],\"zoom\":false}"
    it "encodes default ScatterPlot property names" $ do
      let testInput = def { spX = [1,2,3], spY = [4,5,6] } :: ScatterPlot
        in encode testInput `shouldBe` "{\"points\":[[1,4],[2,5],[3,6]],\"tooltips\":true,\"zoom\":true,\"brush\":true}"
    it "encodes default MatrixPlot property names" $ do
      let testInput = def { mpMatrix = [[1,2,3],[4,5,6],[7,8,9]] } :: MatrixPlot
        in encode testInput `shouldBe` "{\"matrix\":[[1,2,3],[4,5,6],[7,8,9]],\"numbers\":false}"
    it "encodes default Scatter3Plot property names" $ do
      let testInput = def { sptX = [1,2,3], sptY = [4,5,6], sptZ = [7,8,9] } :: Scatter3Plot
        in encode testInput `shouldBe` "{\"points\":[[1,4,7],[2,5,8],[3,6,9]]}"
    it "encodes default MapPlot property names" $ do
      let testInput = def { mppRegions = ["CAN", "USA"], mppWeights = [0.8, 1.0] } :: MapPlot
        in encode testInput `shouldBe` "{\"values\":[0.8,1],\"regions\":[\"CAN\",\"USA\"]}"
    it "encodes default AdjacencyPlot property names" $ do
      let testInput = def { apConn = [[1,2],[3,4]] } :: AdjacencyPlot
        in encode testInput `shouldBe` "{\"symmetric\":true,\"srt\":\"group\",\"numbers\":false,\"links\":[[0,0,1],[0,1,2],[1,0,3],[1,1,4]],\"nodes\":[0,1]}"
    it "encodes default CirclePlot property names" $ do
      let testInput = def { cpConn = [[1,2],[3,4]], cpGroup = Just [1, 1] } :: CirclePlot
        in encode testInput `shouldBe` "{\"group\":[1,1],\"links\":[[0,0,1],[0,1,2],[1,0,3],[1,1,4]],\"nodes\":[0,1]}"
    it "encodes default ForcePlot property names" $ do
      let testInput = def { fpConn = [[1,2,3],[4,5,6],[7,8,9]] } :: ForcePlot
        in encode testInput `shouldBe` "{\"tooltips\":true,\"zoom\":true,\"links\":[[0,0,1],[0,1,2],[0,2,3],[1,0,4],[1,1,5],[1,2,6],[2,0,7],[2,1,8],[2,2,9]],\"nodes\":[0,1,2],\"brush\":true}"
    it "encodes default GraphPlot property names" $ do
      let testInput = def { gpX = [1,2,3], gpY = [4,5,6], gpConn = [[1,2,3],[4,5,6],[7,8,9]] } :: GraphPlot
        in encode testInput `shouldBe` "{\"tooltips\":true,\"zoom\":true,\"links\":[[0,0,1],[0,1,2],[0,2,3],[1,0,4],[1,1,5],[1,2,6],[2,0,7],[2,1,8],[2,2,9]],\"nodes\":[[1,4],[2,5],[3,6]],\"brush\":true}"
