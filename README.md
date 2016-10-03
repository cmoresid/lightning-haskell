# lightning-haskell  

[![Build Status](https://travis-ci.org/cmoresid/lightning-haskell.svg?branch=master)](https://travis-ci.org/cmoresid/lightning-haskell)   

**This is a current work in progress; not ready for usage yet.**  

This package provides Haskell bindings to
[lightning-viz](http://lightning-viz.org/).  

### Basic Usage  

1. Create a session. You can optionally provide the session a name. The variable sId
would contain the session id in the following example.

```
s <- runLightningAnon $ createSession Nothing
let (Right (Session sId _ _ _)) = s
```

2. Create a plot using an existing session. The variable vId will contain the id
of the newly created plot.

```
v <- runLightning (Just sId) $ linePlot $ def { lpSeries = [[1, 2, 3]] }
let (Right (Visualization vId _)) = v
```

3. Putting it all together. (Need to clean this up.)

```
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import Web.Lightning
import Control.Monad.IO.Class
import Web.Lightning.Plots

main :: IO (Either (APIError LightningError) ())
main = runLightning (Just "b948ec69-db3a-4c0d-997c-4c373466628e") $ do
  v <- scatter3Plot def { sptX = [84, 85, 88], sptY = [40, 60, 75], sptZ = [24, 85, 35], sptSize = Just [4], sptLabel = Just [0, 3, 2] }
  liftIO $ T.putStrLn (vizId v)
```
