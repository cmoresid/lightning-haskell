{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.Visualization where

import Data.Aeson
import qualified Data.Text as T

import Network.API.Builder hiding (runRoute)

data Visualization =
  Visualization { vizId        :: T.Text
                , vizSessionID :: T.Text
                } deriving (Show)

instance FromJSON Visualization where
  parseJSON (Object o) =
    Visualization <$> o .: "id"
                  <*> o .: "SessionId"
  parseJSON _ = mempty

instance Receivable Visualization where
  receive = useFromJSON

formatURL :: T.Text -> T.Text
formatURL url =
  case T.last url of
    '/' -> url
    _ -> T.concat [url, "/"]

getPermaLinkURL :: Visualization -> T.Text
getPermaLinkURL viz = T.concat ["/visualizations/", vizId viz]

getLinkType :: Visualization -> T.Text -> T.Text
getLinkType viz link = formatURL $ T.concat [permaLink, link]
  where permaLink = getPermaLinkURL viz

getEmbedLink :: Visualization -> T.Text
getEmbedLink viz = getLinkType viz "/embed"

getIFrameLink :: Visualization -> T.Text
getIFrameLink viz = getLinkType viz "/iframe"

getPymLink :: Visualization -> T.Text
getPymLink viz = getLinkType viz "/pym"

getPublicLink :: Visualization -> T.Text
getPublicLink viz = T.concat [getPermaLinkURL viz, "/public/"]
