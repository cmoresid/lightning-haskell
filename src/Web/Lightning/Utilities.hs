{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Utilities where

import Data.Aeson
import qualified Data.Text as T

omitNulls :: [(T.Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _ = True

createPayLoad :: T.Text -> Value -> Value
createPayLoad t p = object [("type", toJSON t), ("data", p)]

addSessionId :: T.Text -> T.Text -> T.Text
addSessionId url sId = url `T.append` "/sessions/"  `T.append` sId

getLinks :: [[Double]] -> [[Double]]
getLinks _ = [[]]

getNodes :: [[Double]] -> [Int]
getNodes _ = []

getPoints :: [Double] -> [Double] -> [[Double]]
getPoints xs ys = map (\(x, y) -> [x, y]) $ zip xs ys

getPoints3 :: [Double] -> [Double] -> [Double] -> [[Double]]
getPoints3 xs ys zs = map (\(x, y, z) -> [x, y, z]) $ zip3 xs ys zs
