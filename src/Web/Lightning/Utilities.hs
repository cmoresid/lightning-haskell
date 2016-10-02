{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Lightning.Utilities where

import qualified Data.Text as T
import Data.Aeson

addSessionId :: T.Text -> T.Text -> T.Text
addSessionId url sId = url `T.append` "/sessions/"  `T.append` sId

createPayLoad :: T.Text -> Value -> Value
createPayLoad t p = object [("type", toJSON t), ("data", p)]

omitNulls :: [(T.Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _         = True
