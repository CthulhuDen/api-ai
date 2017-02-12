module Data.Aeson.Sparse where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types

sparseObj :: [Pair] -> Value
sparseObj = object . filter notNull
  where
    notNull (_, Null)   = False
    notNull _           = True
