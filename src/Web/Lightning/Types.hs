{-|
Module      : Web.Lightning.Types
Description : Re-exports main types.
Copyright   : (c) Connor Moreside, 2016
License     : BSD-3
Maintainer  : connor@moresi.de
Stability   : experimental
Portability : POSIX

Re-exports all the main types one is likely to need.
-}

module Web.Lightning.Types
  (
    -- * Lightning Types
    LightningError(..)
  , Session(..)
  , Pixel
  , Img
  )
  where

--------------------------------------------------------------------------------
import           Data.Word

import           Web.Lightning.Types.Error   (LightningError (..))
import           Web.Lightning.Types.Session (Session (..))
--------------------------------------------------------------------------------

-- | Can represent an RGBA [red, green, blue, alpha],
-- RGB [red, green, blue], and GreyScale [intensity] pixel.
type Pixel = [Word8]

-- | Represents an image as a matrix of pixels.
type Img = [[Pixel]]
