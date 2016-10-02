{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Web.Lightning.Routes where

import Network.API.Builder hiding (runRoute)

plot :: Route
plot = Route ["visualizations"]
             []
             "POST"
