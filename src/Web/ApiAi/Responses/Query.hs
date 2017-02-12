{-# LANGUAGE TypeFamilies #-}

module Web.ApiAi.Responses.Query where

import ClassyPrelude
import Web.ApiAi.Data.Core
import Web.ApiAi.Responses.Core
import Web.ApiAi.Data.Query
import Web.ApiAi.Data.Intents
import Web.ApiAi.Data.Messages
import Data.Aeson

data ResultSource = Agent | Domain deriving Show

instance FromJSON ResultSource where
    parseJSON = withText "ResultSource" src
      where
        src "agent"     = return Agent
        src "domain"    = return Domain
        src other       = fail $ "Unknown result source: " <> unpack other

data Fulfillment = Fulfillment { ffSpeech :: Text
                               , ffMessages :: [Message]
                               } deriving Show

instance FromJSON Fulfillment where
    parseJSON = withObject "Fulfillment" $ \o -> Fulfillment <$> o .: "speech" <*> o .: "messages"

data QueryResultMetadata = QueryResultMetadata { qrmIntentId :: IntentId
                                               , qrmWebhookUsed :: Text
                                               , qrmWebhookForSlotFillingUsed :: Text
                                               , qrmIntentName :: Text
                                               } deriving Show

instance FromJSON QueryResultMetadata where
    parseJSON = withObject "QueryRequestMetadata" $
                    \o -> QueryResultMetadata <$> o .: "intentId"
                                                <*> o .: "webhookUsed"
                                                <*> o .: "webhookForSlotFillingUsed"
                                                <*> o .: "intentName"

data QueryResult = QueryResult { qResSource :: ResultSource
                               , qResResolvedQuery :: Text
                               , qResAction :: Text
                               , qResActionIncomplete :: Bool
                               , qResParameters :: Parameters
                               , qResContexts :: [QueryContext]
                               , qResFulfillment :: Fulfillment
                               , qResScore :: Double
                               , qResMetadata :: QueryResultMetadata
                               } deriving Show

instance FromJSON QueryResult where
    parseJSON = withObject "QueryResult" $ \o -> QueryResult <$> o .: "source"
                                                                <*> o .: "resolvedQuery"
                                                                <*> o .: "action"
                                                                <*> o .: "actionIncomplete"
                                                                <*> o .: "parameters"
                                                                <*> o .: "contexts"
                                                                <*> o .: "fulfillment"
                                                                <*> o .: "score"
                                                                <*> o .: "metadata"

data QueryResponse = QueryResponse { qRespId :: Text
                                   , qRespTimestamp :: UTCTime
                                   , qRespLang :: Lang
                                   , qRespResult :: QueryResult
                                   , qRespStatus :: ResponseStatus
                                   , qRespSessionId :: SessionId
                                   } deriving Show

instance FromJSON QueryResponse where
    parseJSON = withObject "QueryResponse" $ \o -> QueryResponse <$> o .: "id"
                                                                    <*> o .: "timestamp"
                                                                    <*> o .: "lang"
                                                                    <*> o .: "result"
                                                                    <*> o .: "status"
                                                                    <*> o .: "sessionId"
