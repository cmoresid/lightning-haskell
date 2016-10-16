{-# LANGUAGE OverloadedStrings #-}

module Web.LightningSpec where

import           Test.Hspec

import           Data.Maybe

import           Web.Lightning
import           Web.Lightning.Session

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lightning options API" $ do
    it "allows setting a session name" $ do
      let opts = setSessionName "test" defaultLightningOptions
          sess = fromJust $ optSession opts
       in fromJust (snName sess) `shouldBe` "test"
    it "allows setting a session id" $ do
      let opts = setSessionId "abc-xyz" defaultLightningOptions
          sess = fromJust $ optSession opts
       in snId sess `shouldBe` "abc-xyz"
