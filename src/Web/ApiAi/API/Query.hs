{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.ApiAi.API.Query
    ( ApiAiQueryAPI
    , query
    , queryM
    ) where

import ClassyPrelude
import Web.ApiAi.API.Core
import Web.ApiAi.Data.Core
import Servant.Client
import Servant.API
import Data.Proxy
import Control.Monad.State
import Web.ApiAi.Requests.Query
import Web.ApiAi.Responses.Query
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

type ApiAiQueryAPI = AuthProtect ClientToken :> "query"
                        :> ReqBody '[JSON] (WithDefaultSessionId QueryRequest)
                        :> Post '[JSON] QueryResponse

queryAPI :: Proxy ApiAiQueryAPI
queryAPI = Proxy

query_ :: AuthenticateReq (AuthProtect ClientToken) -> WithDefaultSessionId QueryRequest -> ClientM QueryResponse
query_ = client queryAPI

queryM :: QueryRequest -> ApiAiClient QueryResponse
queryM r = do
    a <- getAuth
    ms <- gets clientSession
    lift $ query_ a $ WithDefaultSessionId r ms

query :: HasClientToken t => t -> SessionId -> QueryRequest -> IO (Either ServantError QueryResponse)
query t s r = runWithState (queryM r) $ ApiAiClientState (getClientToken t) $ Just s
