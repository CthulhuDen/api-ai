{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.ApiAi.Data.Core where

import ClassyPrelude
import Data.Aeson

newtype ClientToken = ClientToken { unClientToken :: Text
                                  } deriving (Show, ToJSON, FromJSON)
newtype DeveloperToken = DeveloperToken { unDeveloperToken :: Text
                                        } deriving (Show, ToJSON, FromJSON)

newtype SessionId = SessionId { unSessionId :: Text
                              } deriving (Show, ToJSON, FromJSON)

data Lang = RussianLang | Lang Text deriving Show

instance ToJSON Lang where
    toJSON RussianLang = toJSON ("ru" :: Text)
    toJSON (Lang l) = toJSON l

instance FromJSON Lang where
    parseJSON = withText "Lang" $ \l -> case l of
                                            "ru" -> return RussianLang
                                            _ -> return $ Lang l

data TimeZone = EuropeMoscow | TimeZone Text deriving Show

instance ToJSON TimeZone where
    toJSON EuropeMoscow = toJSON ("Europe/Moscow" :: Text)
    toJSON (TimeZone tz) = toJSON tz

instance FromJSON TimeZone where
    parseJSON = withText "TimeZone" $ return . tz
      where
        tz "Europe/Moscow"  = EuropeMoscow
        tz other            = TimeZone other

newtype Latitude = Latitude Double deriving (Show, ToJSON, FromJSON)
newtype Longitude = Longitude Double deriving (Show, ToJSON, FromJSON)
data Location = Location Latitude Longitude deriving Show

instance ToJSON Location where
    toJSON (Location lat lon) = object [ "latitude" .= lat
                                       , "longitude" .= lon
                                       ]

instance FromJSON Location where
    parseJSON = withObject "Location" $ \o -> Location <$> o .: "latitude" <*> o .: "longitude"

data Source = Google | Facebook | Kik | Slack | SlackTestbot | Line | Skype
            | Spark | Telegram | Tropo | Twilio | TwilioIp | Twitter deriving Show

instance ToJSON Source where
    toJSON s = toJSON $ str s
      where
        str :: Source -> Text
        str Google = "google"
        str Facebook = "facebook"
        str Kik = "kik"
        str Slack = "slack"
        str SlackTestbot = "slack_testbot"
        str Line = "line"
        str Skype = "skype"
        str Spark = "spark"
        str Telegram = "telegram"
        str Tropo = "tropo"
        str Twilio = "twilio"
        str TwilioIp = "twilio-ip"
        str Twitter = "twitter"

instance FromJSON Source where
    parseJSON = withText "Source" src
      where
        src "google" = return Google
        src "facebook" = return Facebook
        src "kik" = return Kik
        src "slack" = return Slack
        src "slack_testbot" = return SlackTestbot
        src "line" = return Line
        src "skype" = return Skype
        src "spark" = return Spark
        src "telegram" = return Telegram
        src "tropo" = return Tropo
        src "twilio" = return Twilio
        src "twilio-ip" = return TwilioIp
        src "twitter" = return Twitter
        src s = fail $ "Unrecognized source: " <> unpack s

data OriginalRequest = OriginalRequest Source Object deriving Show

instance ToJSON OriginalRequest where
    toJSON (OriginalRequest s d) = object [ "source" .= s
                                          , "data" .= d
                                          ]

instance FromJSON OriginalRequest where
    parseJSON = withObject "OriginalRequest" $ \o -> OriginalRequest <$> o .: "source" <*> o .: "data"

type Parameters = Map Text Text

data WithDefaultSessionId a = WithDefaultSessionId a (Maybe SessionId)
