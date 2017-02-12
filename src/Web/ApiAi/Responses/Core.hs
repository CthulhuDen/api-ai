module Web.ApiAi.Responses.Core where

import ClassyPrelude
import Data.Aeson

data ResponseErrorType = ResponseSuccess | ResponseErrorType Text deriving Show

instance FromJSON ResponseErrorType where
    parseJSON = withText "ResponseErrorType" $ return . errt
      where
        errt "success"  = ResponseSuccess
        errt er         = ResponseErrorType er

data ResponseStatus = ResponseStatus { statusCode :: Int
                                     , statusErrorType :: ResponseErrorType
                                     , statusErrorId :: Maybe Text
                                     , statusErrorDetails :: Maybe Text
                                     } deriving Show

instance FromJSON ResponseStatus where
    parseJSON = withObject "ResponseStatus" $ \o -> ResponseStatus <$> o .: "code"
                                                                    <*> o .: "errorType"
                                                                    <*> o .:? "errorId"
                                                                    <*> o .:? "errorDetails"
