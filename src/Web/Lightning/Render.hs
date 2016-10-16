{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions needed for rendering plots in IHaskell.
module Web.Lightning.Render
  (
    -- * IHaskell Rendering
    renderPlot
  )
  where

--------------------------------------------------------------------------------
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy              as BS
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8)

import           Network.API.Builder.Error
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Text.Blaze.Html                   as BZ
import           Web.Lightning.Types.Visualization (Visualization (..),
                                                    getPublicLink)
--------------------------------------------------------------------------------

-- | For use in IHaskell - Renders the visualization in an IHaskell notebook.
renderPlot :: Either (APIError a) Visualization
              -- ^ The visualization to render.
           -> IO BZ.Markup
              -- ^ The HTML representation of the visualization.
renderPlot (Left _) = return $ BZ.string "No visualization."
renderPlot (Right viz) = do
  manager <- liftIO $ newManager tlsManagerSettings
  req <- parseRequest $ T.unpack $ getPublicLink viz
  vizHtml <- httpLbs req manager

  return $ BZ.preEscapedToMarkup $
    decodeUtf8 $ BS.toStrict $ responseBody vizHtml
