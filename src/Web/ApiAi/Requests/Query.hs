module Web.ApiAi.Requests.Query
    ( EventRequest(..)
    , QueryRequest(..)
    , queryRequest
    ) where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types ( Pair )
import Web.ApiAi.Data.Core
import Web.ApiAi.Data.Entities
import Web.ApiAi.Data.Query

data BaseQueryRequest = BaseQueryRequest { bqReqV :: Text
                                         , bqReqSessionId :: Maybe SessionId
                                         -- ^ Optional because can also be set in state
                                         , bqReqLang :: Lang
                                         , bqReqContexts :: Maybe [QueryContextRequest]
                                         , bqReqResetContext :: Maybe Bool
                                         , bqReqEntities :: Maybe [Entity]
                                         , bqReqTimezone :: Maybe TimeZone
                                         , bqReqLocation :: Maybe Location
                                         , bqReqOriginalRequest :: Maybe OriginalRequest
                                         } deriving Show

bqrPairs :: BaseQueryRequest -> Maybe SessionId -> [Pair]
bqrPairs bqr mSid = [ "v" .= bqReqV bqr
                    , "sessionId" .= (mSid <|> bqReqSessionId bqr)
                    , "lang" .= bqReqLang bqr
                    , "contexts" .= bqReqContexts bqr
                    , "resetContexts" .= bqReqResetContext bqr
                    , "entities" .= bqReqEntities bqr
                    , "timezone" .= bqReqTimezone bqr
                    , "location" .= bqReqLocation bqr
                    , "originalRequest" .= bqReqOriginalRequest bqr
                    ]

instance ToJSON (WithDefaultSessionId BaseQueryRequest) where
    toJSON (WithDefaultSessionId bqr ms) = object $ bqrPairs bqr ms

data EventRequest = EventRequest { eeqReqName :: Text
                                 , eeqReqData :: Maybe Parameters
                                 } deriving Show

instance ToJSON EventRequest where
    toJSON (EventRequest n d) = object [ "name" .= n
                                       , "data" .= d
                                       ]

data QueryRequest = QueryRequest      { qReqQuery :: Text
                                      , qReqEvent :: Maybe EventRequest
                                      , qReqRest :: BaseQueryRequest
                                      }
                  | EventQueryRequest { eqReqQuery :: Maybe Text
                                      , eqReqEvent :: EventRequest
                                      , eqReqRest :: BaseQueryRequest
                                      }
                  deriving Show

instance ToJSON (WithDefaultSessionId QueryRequest) where
    toJSON (WithDefaultSessionId (QueryRequest q e r) ms)
                = object $ [ "query" .= q
                           , "event" .= e
                           ] <> bqrPairs r ms
    toJSON (WithDefaultSessionId (EventQueryRequest q e r) ms)
                = object $ [ "query" .= q
                           , "event" .= e
                           ] <> bqrPairs r ms

versionDate :: Text
versionDate = "20170212"

defaultLang :: Lang
defaultLang = RussianLang

queryRequest :: Text -> QueryRequest
queryRequest q = QueryRequest q Nothing $ BaseQueryRequest versionDate Nothing defaultLang
                                                    Nothing Nothing Nothing Nothing Nothing Nothing
