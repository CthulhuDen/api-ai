{-# LANGUAGE TypeOperators #-}

module Web.ApiAi.API
    ( module Import
    , ApiAiAPI
    ) where

import Servant.API
import Web.ApiAi.API.Core as Import
import Web.ApiAi.API.Entities as Import
import Web.ApiAi.API.Query as Import

type ApiAiAPI = ApiAiEntitiesAPI :<|> ApiAiQueryAPI
