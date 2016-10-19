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
    -- * Validation Functions
  , validateBin
  , validateColor
  , validateColorMap
  , validateSize
  , validateAlpha
  , validateThickness
  , validateIndex
  , validateCoordinates
  , validateCoordinates3
  , validateRegion
  , validateConn
  )
  where

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text                 as T

import           Web.Lightning.Types.Error
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
  | length conn == length (head conn) = [0..length conn - 1]
  | otherwise                         = [0..n - 1]
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

-- | Validates the bins of a histogram
validateBin :: Maybe [Double]
             -> Either LightningError (Maybe [Double])
validateBin = return

-- | Verify that the color specs are either in the form of
-- [r, g, b] or a list of [[r,g,b],[r,g,b],...]
validateColor :: Maybe [Int]
               -> Either LightningError (Maybe [Int])
validateColor (Just colors)
  | length colors == 3 = Right (Just colors)
  | otherwise          = Left $ ValidationError "Color must have three values."
validateColor Nothing = Right Nothing

-- | Verifiy that the color map specified is on of the colorbrewer maps.
--
-- Here are the available colorbrewer values:
-- "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
-- "RdYlGn", "Spectral", "Blues", "BuGn", "BuPu", "GnBu",
-- "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
-- "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
-- "YlOrBr", "YlOrRd", "Accent", "Dark2", "Paired", "Pastel1",
-- "Pastel2", "Set1", "Set2", "Set3", or "Lightning"
validateColorMap :: Maybe T.Text
                 -> Either LightningError (Maybe T.Text)
validateColorMap cm@(Just cmv) =
  if cmv `elem` colorMaps
    then Right cm
    else Left $ ValidationError "Invalid color map specified."
  where colorMaps = ["BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
                     "RdYlGn", "Spectral", "Blues", "BuGn", "BuPu", "GnBu",
                     "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
                     "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
                     "YlOrBr", "YlOrRd", "Accent", "Dark2", "Paired", "Pastel1",
                     "Pastel2", "Set1", "Set2", "Set3", "Lightning"]
validateColorMap Nothing = Right Nothing

-- | Verify that all size values are greater than zero.
validateSize :: Maybe [Int]
             -> Either LightningError (Maybe [Int])
validateSize size = validateGreaterThan0 size msg
  where msg = "Sizes cannot be 0 or negative."

-- | Verify all alpha values are greater than 0
validateAlpha :: Maybe [Double]
              -> Either LightningError (Maybe [Double])
validateAlpha alpha = validateGreaterThan0 alpha msg
  where msg = "Alpha cannot be 0 or negative."

-- | Verify all thickness values are greater than 0.
validateThickness :: Maybe [Double]
                  -> Either LightningError (Maybe [Double])
validateThickness thickness = validateGreaterThan0 thickness msg
  where msg = "Thickness cannot be 0 or negative."

-- | Verify that there is at least one element in list.
validateIndex :: Maybe [Int]
              -> Either LightningError (Maybe [Int])
validateIndex index@(Just idx)
  | not (null idx) = Right index
  | otherwise      = Left $ ValidationError "Index must be non-singleton."
validateIndex Nothing = Right Nothing

-- | Verify that the length of vector x is equal to length of vector y.
validateCoordinates :: [Double]
                        -- ^ x vector
                    -> [Double]
                        -- ^ y vector
                    -> Either LightningError ([Double], [Double])
validateCoordinates xs ys =
  if length xs == length ys
    then Right (xs, ys)
    else Left $ ValidationError "x and y vectors must be the same length."

-- | Verify that the lenths of the x, y, and z vectors are the same length.
validateCoordinates3 :: [Double]
                        -- ^ x vector
                     -> [Double]
                        -- ^ y vector
                     -> [Double]
                        -- ^ z vector
                     -> Either LightningError ([Double],[Double],[Double])
validateCoordinates3 xs ys zs =
  if (length xs == length ys) && (length ys == length zs)
    then Right (xs, ys, zs)
    else Left $ ValidationError "x, y, and z vectors must be the same length."

-- | Verify that the lengths of the region names are either 2 letters or 3
-- letters.
--
-- 2 letter region names must correspond to US states and 3 letter
-- region names must correspond to countries of the world.
validateRegion :: Maybe [T.Text]
               -> Either LightningError (Maybe [T.Text])
validateRegion regions@(Just rs) =
  if checkTwo || checkThree
    then Right regions
    else Left $ ValidationError msg
  where
    msg = "All region names must be all 2 letters or all 3 letters."
    checkTwo = all (\x -> T.length x == 2) rs
    checkThree = all (\x -> T.length x == 3) rs
validateRegion Nothing = Right Nothing

-- | Verify that the multi-dimensional list adheres to expected dimensions.
validateConn :: [[Double]]
             -> Either LightningError [[Double]]
validateConn conn
  | length conn == length (head conn) = Right conn
  | length (head conn) == 2           = Right conn
  | length (head conn) == 3           = Right conn
  | otherwise                         = Left $ ValidationError msg
  where
    msg = "Too many entries per link, must be 2 or 3."

-- | Ensure all values are greater than 0.
validateGreaterThan0 :: (Ord a, Num a) => Maybe [a]
                                         -> T.Text
                                         -> Either LightningError (Maybe [a])
validateGreaterThan0 vals@(Just vs) msg =
  if any (<= 0) vs
    then Left $ ValidationError msg
    else Right vals
validateGreaterThan0 Nothing _ = Right Nothing
