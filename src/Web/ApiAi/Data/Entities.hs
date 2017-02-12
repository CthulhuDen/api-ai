{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.ApiAi.Data.Entities where

import ClassyPrelude
import Data.Aeson
import Servant.API

data EntityEntry = EntityEntry { eeValue    :: Text
                               , eeSynonyms :: [Text]
                               } deriving Show

instance FromJSON EntityEntry where
    parseJSON = withObject "EntityEntry" $ \o -> EntityEntry   <$> o .: "value"
                                                                <*> o .: "synonyms"

instance ToJSON EntityEntry where
    toJSON (EntityEntry v s) = object [ "value" .= v
                                      , "synonyms" .= s
                                      ]

newtype EntityId = EntityId { unEntityId :: Text
                            } deriving ( Show, ToHttpApiData, FromHttpApiData, FromJSON, ToJSON )

data EntityPreview = EntityPreview { epId       :: EntityId
                                   , epName     :: Text
                                   , epCount    :: Int
                                   , epPreview  :: Text
                                   } deriving Show

instance FromJSON EntityPreview where
    parseJSON = withObject "EntityPreview" $ \o -> EntityPreview    <$> o .: "id"
                                                                    <*> o .: "name"
                                                                    <*> o .: "count"
                                                                    <*> o .: "preview"

data Entity = Entity { eId                  :: EntityId
                     , eName                :: Text
                     , eEntries             :: [EntityEntry]
                     , eIsEnum              :: Bool
                     , eAutomatedExpansion  :: Bool
                     } deriving Show

instance FromJSON Entity where
    parseJSON = withObject "Entity" $ \o -> Entity  <$> o .: "id"
                                                    <*> o .: "name"
                                                    <*> o .: "entries"
                                                    <*> o .: "isEnum"
                                                    <*> o .: "automatedExpansion"

instance ToJSON Entity where
    toJSON (Entity i n e ie a) = object [ "id" .= i
                                        , "name" .= n
                                        , "entries" .= e
                                        , "isEnum" .= ie
                                        , "automatedExpansion" .= a
                                        ]
