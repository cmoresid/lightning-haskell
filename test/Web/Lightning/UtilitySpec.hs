{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.UtilitySpec where

import           Test.Hspec

import           Data.Aeson

import           Web.Lightning.Plots
import           Web.Lightning.Utilities

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lightning Utilities" $ do
    it "createPayLoad should wrap plot and type in JSON object" $ do
      let plotValue = toJSON $ def { lpSeries = [[1,2,3]]}
          payload = createPayLoad "line" plotValue
       in encode payload `shouldBe` "{\"data\":{\"series\":[[1,2,3]],\"zoom\":true},\"type\":\"line\"}"
    it "addSessionId appends session ID to URL" $ do
      addSessionId defaultBaseURL "1b95a317-088e-4deb-a415-76962356742b" `shouldBe` "http://localhost:3000/sessions/1b95a317-088e-4deb-a415-76962356742b"
    it "getPoints zips x and y points into x-y co-ordinates" $ do
      getPoints [1,2,3] [4,5,6] `shouldBe` [[1.0,4.0],[2.0,5.0],[3.0,6.0]]
    it "getPoints3 zips x, y, and z points into xyz co-ordinates" $ do
      getPoints3 [1,2,3] [4,5,6] [7,8,9] `shouldBe` [[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0]]
    it "getNodes # of nodes from adjacency matrix" $ do
      getNodes [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` [0,1,2]
    it "getLinks converts adjacency matrix to [source, target, value] form." $ do
      getLinks [[1,2],[3,4]] `shouldBe` [[0.0,0.0,1.0],[0.0,1.0,2.0],[1.0,0.0,3.0],[1.0,1.0,4.0]]

  describe "Plot Validation Utilities" $ do
    it "validateBin should be idempotent with Just value." $ do
      let test = validateBin $ Just [1.0,2.0,3.0]
        in test `shouldSatisfy` isRight
    it "validateBin should be idempotent with Nothing value." $ do
      let test = validateBin Nothing
        in test `shouldSatisfy` isRight
    it "validateColor should be IsRight with [r,g,b] value." $ do
      let test = validateColor $ Just [255,0,0]
        in test `shouldSatisfy` isRight
    it "validateColor should be IsLeft with [a,b,c,d] value." $ do
      let test = validateColor $ Just [255,0,0,0]
        in test `shouldSatisfy` isLeft
    it "validateColorMap should be IsRight with valid color map." $ do
      let test = validateColorMap $ Just "BrBG"
        in test `shouldSatisfy` isRight
    it "validateColorMap should be IsLeft with invalid color map." $ do
      let test = validateColorMap $ Just "NotValidColorMap"
        in test `shouldSatisfy` isLeft
    it "validateSize should be IsRight with positive values." $ do
      let test = validateSize $ Just [1,2,3]
        in test `shouldSatisfy` isRight
    it "validateSize should be IsLeft with 0 or below values." $ do
      let test = validateSize $ Just [0,0,-1]
        in test `shouldSatisfy` isLeft
    it "validateAlpha should be IsRight with positive values." $ do
      let test = validateAlpha $ Just [1,2,3]
        in test `shouldSatisfy` isRight
    it "validateAlpha should be IsLeft with 0 or below values." $ do
      let test = validateAlpha $ Just [-10,3,0]
        in test `shouldSatisfy` isLeft
    it "validateThickness should be IsRight with positive values." $ do
      let test = validateThickness $ Just [1,2,3]
        in test `shouldSatisfy` isRight
    it "validateThickness should be IsLeft with 0 or below values." $ do
      let test = validateThickness $ Just [-10,3,0]
        in test `shouldSatisfy` isLeft
    it "validateIndex should be IsRight with more than 1 element." $ do
      let test = validateIndex $ Just [1,2,3]
        in test `shouldSatisfy` isRight
    it "validateIndex should be IsLeft with no elements." $ do
      let test = validateIndex $ Just []
        in test `shouldSatisfy` isLeft
    it "validateCoordinates should be IsRight with equal vector lengths." $ do
      let test = validateCoordinates [1.0,2.0,3.0] [4.0,5.0,6.0]
        in test `shouldSatisfy` isRight
    it "validateCoordinates should be IsLeft with unqual vector lengths." $ do
      let test = validateCoordinates [1.0,2.0,3.0] [1.0,2.0]
        in test `shouldSatisfy` isLeft
    it "validateCoordinates3 should be IsRight with equal vector lengths." $ do
      let test = validateCoordinates3 [1.0,2.0,3.0] [4.0,5.0,6.0] [7.0,8.0,9.0]
        in test `shouldSatisfy` isRight
    it "validateCoordinates3 should be IsLeft with unqual vector lengths." $ do
      let test = validateCoordinates3 [1.0,2.0,3.0] [1.0,2.0] [1.0,2.0,3.0]
        in test `shouldSatisfy` isLeft
    it "validateRegion should be IsRight with all 2 letter words." $ do
      let test = validateRegion ["AL","AZ"]
        in test `shouldSatisfy` isRight
    it "validateRegion should be IsRight with all 3 letter words." $ do
      let test = validateRegion ["CAN","USA","MEX"]
        in test `shouldSatisfy` isRight
    it "validateRegion should be IsLeft with different word lengths." $ do
      let test = validateRegion ["CAN","AL"]
        in test `shouldSatisfy` isLeft
    it "validateConn should be IsRight with symmetric matrix." $ do
      let test = validateConn [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]
        in test `shouldSatisfy` isRight
    it "validateConn should be IsRight with node links." $ do
      let test = validateConn [[1.0,2.0],[2.0,3.0],[3.0,4.0]]
        in test `shouldSatisfy` isRight
    it "validateConn should be IsRight with node links and weights." $ do
      let test = validateConn [[1.0,2.0,0.6],[1.0,2.0,0.1]]
        in test `shouldSatisfy` isRight
    it "validateConn should be IsLeft with too many values in node pairing" $ do
      let test = validateConn [[1.0,2.0,3.0,4.0]]
        in test `shouldSatisfy` isLeft
