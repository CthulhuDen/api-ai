{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.ApiAi.API.Entities
    ( ApiAiEntitiesAPI
    , getEntities
    , getEntitiesM
    , getEntity
    , getEntityM
    ) where

import ClassyPrelude
import Web.ApiAi.API.Core
import Servant.Client
import Servant.API
import Web.ApiAi.Data.Entities
import Data.Proxy
import Control.Monad.State
import Network.HTTP.Client ( Manager )

type ApiAiEntitiesAPI = AuthProtect DeveloperToken :> "entities" :> Get '[JSON] [EntityPreview]
                        :<|> AuthProtect DeveloperToken :> "entities" :> Capture "entityId" EntityId :> Get '[JSON] Entity

entitiesAPI :: Proxy ApiAiEntitiesAPI
entitiesAPI = Proxy

getEntities_ :: AuthenticateReq (AuthProtect DeveloperToken) -> ClientM [EntityPreview]
getEntity_ :: AuthenticateReq (AuthProtect DeveloperToken) -> EntityId -> ClientM Entity
getEntities_ :<|> getEntity_ = client entitiesAPI

getEntities :: HasDeveloperToken t => t -> IO (Either ServantError [EntityPreview])
getEntities t = runWithToken getEntitiesM $ getDeveloperToken t

getEntitiesM :: ApiAiDeveloper [EntityPreview]
getEntitiesM = auth getEntities_ >>= lift

getEntity :: HasDeveloperToken t => t -> EntityId -> IO (Either ServantError Entity)
getEntity t eid = runWithToken (getEntityM eid) $ getDeveloperToken t

getEntityM :: EntityId -> ApiAiDeveloper Entity
getEntityM eid = auth getEntity_ >>= lift . ($ eid)
