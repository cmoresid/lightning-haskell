{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Lightning.Render
  (
    renderPlot
  )
  where

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

renderPlot :: Either (APIError a) Visualization -> IO BZ.Markup
renderPlot (Left _) = return $ BZ.string "No visualization."
renderPlot (Right viz) = do
  manager <- liftIO $ newManager tlsManagerSettings
  req <- parseRequest $ T.unpack $ getPublicLink viz
  vizHtml <- httpLbs req manager

  return $ BZ.preEscapedToMarkup $
    decodeUtf8 $ BS.toStrict $ responseBody vizHtml
