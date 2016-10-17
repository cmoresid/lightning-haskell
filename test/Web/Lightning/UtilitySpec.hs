{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.UtilitySpec where

import           Test.Hspec

import           Data.Aeson

import           Web.Lightning.Plots
import           Web.Lightning.Utilities
import           Web.Lightning.Types.Lightning(defaultBaseURL)

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
