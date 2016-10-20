{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.PlotsSpec where

import           Test.Hspec

import           Data.Aeson
import           Data.Maybe

import           Web.Lightning.Plots
import           Web.Lightning.Types.Lightning (validatePlot)

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

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

  describe "Lighting Plot Validation" $ do
    it "should be IsRight for valid AdjacencyPlot." $ do
      let testInput = validatePlot $ def { apConn = [[1,2,3],[4,5,6],[7,8,9]] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid AdjacencyPlot." $ do
      let testInput = validatePlot $ def { apConn = [[], [1,2,3,4]] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid CirclePlot." $ do
      let testInput = validatePlot $ def { apConn = [[1,2],[3,4]] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid CirclePlot." $ do
      let testInput = validatePlot $ def { cpConn = [[1,2],[3,4]], cpColor = Just [1,2] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid ForcePlot." $ do
      let testInput = validatePlot $ def { fpConn = [[1,2,3],[1,2,3],[1,2,3]], fpBrush = Just False }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid ForcePlot." $ do
      let testInput = validatePlot $ def { fpConn = [[], [1.0]] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid GraphPlot." $ do
      let testInput = validatePlot $ def { gpX = [1], gpY = [1], gpConn = [[1,2,3]] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid GraphPlot." $ do
      let testInput = validatePlot $ def { gpX = [1], gpY = [1,2,3], gpConn = [[1,2,3]] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for HistogramPlot." $ do
      let testInput = validatePlot $ def { hpValues = [1,2,1,1,1,4], hpBins = Just [1,2,3] }
        in testInput `shouldSatisfy` isRight
    it "should be IsRight for valid LinePlot." $ do
      let testInput = validatePlot $ def { lpSeries = [[1,2,3]], lpColor = Just [244,0,100] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid LinePlot." $ do
      let testInput = validatePlot $ def { lpSeries = [[1,2,3,4]], lpColor = Just [0] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid MapPlot." $ do
      let testInput = validatePlot $ def { mppRegions = ["USA","CAN"], mppWeights = [0.2,1.3] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid MapPlot." $ do
      let testInput = validatePlot $ def { mppRegions = ["USA","CAN", "AL"], mppWeights = [0.2,1.3] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid MatrixPlot." $ do
      let testInput = validatePlot $ def { mpMatrix = [[1,2,3],[4,5,6]] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid MatrixPlot." $ do
      let testInput = validatePlot $ def { mpMatrix = [[1,2,3],[4,5,6],[7,8,9]], mpColorMap = Just "NotAColorMap" }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid ScatterPlot." $ do
      let testInput = validatePlot $ def { spX = [1,2,3], spY = [1,2,3] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid ScatterPlot." $ do
      let testInput = validatePlot $ def { spX = [1,2,3], spY = [0,0,0], spAlpha = Just [-1,0,1] }
        in testInput `shouldSatisfy` isLeft
    it "should be IsRight for valid ScatterPlot3." $ do
      let testInput = validatePlot $ def { sptX = [1,2,3], sptY = [1,2,3], sptZ = [0,0,0] }
        in testInput `shouldSatisfy` isRight
    it "should be IsLeft for invalid ScatterPlot3." $ do
      let testInput = validatePlot $ def { sptX = [1,2,3], sptY = [0,0,0], sptZ = [1,2,3,4] }
        in testInput `shouldSatisfy` isLeft
