module Web.ApiAi.Data.Query where

import ClassyPrelude
import Web.ApiAi.Data.Core
import Data.Aeson
import Data.Aeson.Sparse

data QueryContextRequest = QueryContextRequest { qcrName :: Text
                                               , qcrParameters :: Maybe Parameters
                                               , qcrLifespan :: Maybe Int
                                               } deriving Show

instance ToJSON QueryContextRequest where
    toJSON (QueryContextRequest n p l) = sparseObj [ "name" .= n
                                                  , "parameters" .= p
                                                  , "lifespan" .= l
                                                  ]

instance FromJSON QueryContextRequest where
    parseJSON = withObject "QueryContextRequest" $ \o -> QueryContextRequest <$> o .: "name"
                                                                                <*> o .:? "parameters"
                                                                                <*> o .:? "lifespan"

data QueryContext = QueryContext { qcName :: Text
                                 , qcParameters :: Parameters
                                 , qcLifespan :: Int
                                 } deriving Show

instance ToJSON QueryContext where
    toJSON (QueryContext n p l) = object [ "name" .= n
                                         , "parameters" .= p
                                         , "lifespan" .= l
                                         ]

instance FromJSON QueryContext where
    parseJSON = withObject "QueryContext" $ \o -> QueryContext <$> o .: "name"
                                                                <*> o .: "parameters"
                                                                <*> o .: "lifespan"
