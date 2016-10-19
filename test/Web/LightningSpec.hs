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
    it "allows setting Lightning's API base URL" $ do
      let opts = setBaseURL "http://127.0.0.1:3000" defaultLightningOptions
       in optHostUrl opts `shouldBe` "http://127.0.0.1:3000"
    it "allows setting a session name" $ do
      let opts = setSessionName "test" defaultLightningOptions
          sess = fromJust $ optSession opts
       in fromJust (snName sess) `shouldBe` "test"
    it "allows setting a session id" $ do
      let opts = setSessionId "abc-xyz" defaultLightningOptions
          sess = fromJust $ optSession opts
       in snId sess `shouldBe` "abc-xyz"
    it "allows setting HTTP Basic Auth credentials" $ do
      let opts = setBasicAuth ("user", "secret") defaultLightningOptions
          (BasicAuth (_, p)) = optLoginMethod opts
       in p `shouldBe` "secret"
