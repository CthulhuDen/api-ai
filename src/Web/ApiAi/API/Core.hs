{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Web.ApiAi.API.Core
    ( SessionId (..)
    , ClientToken (..)
    , DeveloperToken (..)
    , ApiAiClientState (..)
    , ApiAiDeveloperState (..)
    , ApiAiClient
    , ApiAiDeveloper
    , HasClientToken (..)
    , HasDeveloperToken (..)
    , HasToken (..)
    , RunsWithState (..)
    , getAuth
    , auth
    , withToken
    , runWithToken
    ) where

import ClassyPrelude
import Servant.API hiding ( addHeader )
import Servant.Client
import Servant.Common.Req ( Req, addHeader )
import Control.Monad.State
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Web.ApiAi.Data.Core

data ApiAiClientState = ApiAiClientState { clientToken :: ClientToken
                                         , clientSession :: Maybe SessionId
                                         } deriving Show

data ApiAiDeveloperState = ApiAiDeveloperState { developerToken :: DeveloperToken
                                      } deriving Show

class FromToken s where
    type StateToken s :: *
    withToken :: StateToken s -> s

instance FromToken ApiAiClientState where
    type StateToken ApiAiClientState = ClientToken
    withToken t = ApiAiClientState t Nothing

instance FromToken ApiAiDeveloperState where
    type StateToken ApiAiDeveloperState = DeveloperToken
    withToken = ApiAiDeveloperState

type ApiAiClient = StateT ApiAiClientState ClientM
type ApiAiDeveloper = ReaderT ApiAiDeveloperState ClientM

apiAiBaseUrl :: BaseUrl
apiAiBaseUrl = BaseUrl Https "api.api.ai" 443 "/v1"

class RunsWithState m where
    type MonadStateType m :: *
    runWithState :: m a -> MonadStateType m -> IO (Either ServantError a)

instance RunsWithState ApiAiClient where
    type MonadStateType ApiAiClient = ApiAiClientState
    runWithState acm st = do
        manager <- newManager tlsManagerSettings
        runClient' acm st (ClientEnv manager apiAiBaseUrl)

instance RunsWithState ApiAiDeveloper where
    type MonadStateType ApiAiDeveloper = ApiAiDeveloperState
    runWithState adm st = do
        manager <- newManager tlsManagerSettings
        runDeveloper' adm st (ClientEnv manager apiAiBaseUrl)

-- | Allows to run any ApiAiClient or ApiAiDeveloper actions with given token
-- If you want to run against the complete state, use runWithState instead
runWithToken :: (FromToken s, RunsWithState m, MonadStateType m ~ s, StateToken s ~ t)
             => m a -> t -> IO (Either ServantError a)
runWithToken am = runWithState am . withToken

-- | Allows to run 'ApiAiClient' against arbitrary url
runClient' :: ApiAiClient a -> ApiAiClientState -> ClientEnv -> IO (Either ServantError a)
runClient' acm st = runClientM (evalStateT acm st)

-- | Allows to run 'ApiAiDeveloper' against arbitrary url
runDeveloper' :: ApiAiDeveloper a -> ApiAiDeveloperState -> ClientEnv -> IO (Either ServantError a)
runDeveloper' acm st = runClientM (runReaderT acm st)

-- | Request authorization

type instance AuthClientData (AuthProtect DeveloperToken) = DeveloperToken
type instance AuthClientData (AuthProtect ClientToken) = ClientToken

class HasClientToken a where
    getClientToken :: a -> ClientToken

instance HasClientToken ClientToken where
    getClientToken = id

instance HasClientToken ApiAiClientState where
    getClientToken = clientToken

class HasDeveloperToken a where
    getDeveloperToken :: a -> DeveloperToken

instance HasDeveloperToken DeveloperToken where
    getDeveloperToken = id

instance HasDeveloperToken ApiAiDeveloperState where
    getDeveloperToken = developerToken

class Monad m => HasToken m t where
    getToken :: m t

instance HasToken ApiAiClient ClientToken where
    getToken = gets clientToken

instance HasToken ApiAiDeveloper DeveloperToken where
    getToken = asks developerToken

class ToText t where
    toText :: t -> Text

instance ToText ClientToken where
    toText = unClientToken

instance ToText DeveloperToken where
    toText = unDeveloperToken

auth' :: Text -> Req -> Req
auth' t = addHeader "Authorization" $ "Bearer " <> t

getAuth :: (HasToken m t, ToText t, AuthClientData (AuthProtect t) ~ t)
     => m (AuthenticateReq (AuthProtect t))
getAuth = flip mkAuthenticateReq (auth' . toText) <$> getToken

auth :: (HasToken m t, ToText t, AuthClientData (AuthProtect t) ~ t)
     => (AuthenticateReq (AuthProtect t) -> a) -> m a
auth f = f <$> getAuth
