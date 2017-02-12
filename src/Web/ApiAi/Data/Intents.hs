{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.ApiAi.Data.Intents where

import ClassyPrelude
import Data.Aeson
import Servant.API

newtype IntentId = IntentId { unIntentId :: Text
                            } deriving ( Show, ToHttpApiData, FromHttpApiData, FromJSON, ToJSON )
