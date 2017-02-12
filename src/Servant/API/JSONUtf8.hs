{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.API.JSONUtf8 where

import ClassyPrelude
import Servant.API.ContentTypes
import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Network.HTTP.Media as M

data JSONUtf8 deriving Typeable

-- | @application/json@
instance Accept JSONUtf8 where
    contentType _ = "application" M.// "json" M./: ("charset", "utf-8")

-- | `encode`
instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender JSONUtf8 a where
    mimeRender _ = encode

-- | `eitherDecode`
instance FromJSON a => MimeUnrender JSONUtf8 a where
    mimeUnrender _ = eitherDecodeLenient
