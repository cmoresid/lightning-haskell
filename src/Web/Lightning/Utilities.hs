{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Lightning.Utilities
Description : Commonly used utility functions.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Contains commonly used utility functions used throughout the library.
-}

module Web.Lightning.Utilities
  (
    -- * Utility Functions
    omitNulls
  , createPayLoad
  , addSessionId
  , getLinks
  , getNodes
  , getPoints
  , getPoints3
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text  as T
--------------------------------------------------------------------------------

-- | Used in conjunction with ToJSON. It will stop any field that is
-- Nothing (null) in a record from being encoded in JSON.
omitNulls :: [(T.Text, Value)]
             -- ^ The plot object to be serialized into JSON Value
          -> Value
             -- ^ The plot object with Nothing (null) fields removed.
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _         = True

-- | Converts the plot creation request record into the proper
-- JSON object that the lightning-viz server expects.
createPayLoad :: T.Text
                 -- ^ The name of the type of plot to create
              -> Value
                 -- ^ The plot creation request record in JSON Value format
              -> Value
                 -- ^ The properly payload object
createPayLoad t p = object [("type", toJSON t), ("data", p)]

-- | Appends the session route and session id to current URL.
addSessionId :: T.Text
                -- ^ The current URL
             -> T.Text
                -- ^ The session ID to add to current URL
             -> T.Text
                -- ^ Returns the URL with the session ID
addSessionId url sId = url `T.append` "/sessions/"  `T.append` sId

-- | Retrieves all the links for each of the nodes in the adjacency
-- matrix.
getLinks :: [[Double]]
            -- ^ The adjacency matrix
         -> [[Double]]
            -- ^ All the links for each of the nodes
getLinks conn
  | length conn == length (head conn) = s1 (zipWithIndex conn)
  | otherwise                         = s4 conn
  where s1 = concatMap (\(row, i) -> s3 i (s2 (zipWithIndex row)))
        s2 = filter (\(x, _) -> x /= 0)
        s3 i = map (\(x, j) -> [i, j, x] :: [Double])
        s4 xs = case length xs of
          2 -> xs
          3 -> map (\l -> [head l, l !! 1, 1.0]) xs
          _ -> [[]]

zipWithIndex :: (Enum b, Num b) => [a] -> [(a, b)]
zipWithIndex [] = []
zipWithIndex xs = zipWith (\i el -> (i, el)) xs [0..]

-- | Retrieves all of the nodes from an adjacency matrix.
getNodes :: [[Double]]
            -- ^ The adjacency matrix
         -> [Int]
            -- ^ A list of all of the nodes in matrix
getNodes conn
  | length conn == length (head conn) = [0..length conn]
  | otherwise                         = [0..n]
  where n = floor $ maximum $ map maximum conn


-- | Zips up x and y points into array pairs.
getPoints :: [Double]
             -- ^ X points
          -> [Double]
             -- ^ Y points
          -> [[Double]]
             -- ^ Returns [ [x, y] ] pairs
getPoints xs ys = map (\(x, y) -> [x, y]) $ zip xs ys

-- | Zips up x, y, and z points into array triples.
getPoints3 :: [Double]
              -- ^ X points
           -> [Double]
              -- ^ Y points
           -> [Double]
              -- ^ Z points
           -> [[Double]]
              -- ^ Returns [ [x, y, z] ] triplets
getPoints3 xs ys zs = map (\(x, y, z) -> [x, y, z]) $ zip3 xs ys zs
