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
  ( LightningError(..)
  , Session(..)
  ) where

import Web.Lightning.Types.Error (LightningError(..))
import Web.Lightning.Types.Session (Session(..))
