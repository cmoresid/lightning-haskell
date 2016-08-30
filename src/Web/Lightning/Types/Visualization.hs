{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Types.Visualization where

import qualified Data.Text as T

data Visualization =
  Visualization { vizId        :: T.Text
                , vizName      :: T.Text
                , vizHost      :: T.Text
                , vizSessionID :: T.Text
                }

formatURL :: Visualization -> T.Text -> T.Text
formatURL viz url =
  let out = case T.last url of
              '/' -> url
              _ -> T.concat [url, "/"]
  in T.concat [out, "?host=", vizHost viz]

getPermaLinkURL :: Visualization -> T.Text
getPermaLinkURL viz = T.concat [vizHost viz, "/visualizations/", vizId viz]

getLinkType :: Visualization -> T.Text -> T.Text
getLinkType viz link = formatURL viz $ T.concat [permaLink, link]
  where permaLink = getPermaLinkURL viz

getEmbedLink :: Visualization -> T.Text
getEmbedLink viz = getLinkType viz "/embed"

getIFrameLink :: Visualization -> T.Text
getIFrameLink viz = getLinkType viz "/iframe"

getPymLink :: Visualization -> T.Text
getPymLink viz = getLinkType viz "/pym"

getDataLink :: Visualization -> T.Text
getDataLink viz =
  formatURL viz $ T.concat [vizHost viz, "/sessions/",
                            vizSessionID viz, "/visualizations/",
                            vizId viz, "/data/"]

getPublicLink :: Visualization -> T.Text
getPublicLink viz = T.concat [getPermaLinkURL viz, "/public/"]
