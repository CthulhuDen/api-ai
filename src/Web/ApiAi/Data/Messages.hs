{-# LANGUAGE ScopedTypeVariables #-}

module Web.ApiAi.Data.Messages where

import ClassyPrelude
import Data.Aeson

data CardMessageButton = CardMessageButton { cmBtnText :: Text
                                           , cmBtnPostback :: Text
                                           } deriving Show

instance ToJSON CardMessageButton where
    toJSON (CardMessageButton t p) = object [ "text" .= t
                                            , "postback" .= p
                                            ]

instance FromJSON CardMessageButton where
    parseJSON = withObject "CarMessageButton" $ \o -> CardMessageButton <$> o .: "text" <*> o .: "postback"

data Message = TextMessage { tMsgSpeech :: Text }
             | ImageMessage { iMsgImageUrl :: Text }
             | CardMessage { cMsgTitle :: Text
                           , cMsgSubtitle :: Text
                           , cMsgButtons :: [CardMessageButton]
                           }
             | QuickRepliesMessage { qrMsgTitle :: Text
                                   , qrMsgReplies :: [Text]
                                   }
             | CustomPayloadMessage { cpMsgPayload :: Object
                                    }
             deriving Show

instance ToJSON Message where
    toJSON (TextMessage s) = object [ "type" .= (0 :: Int)
                                    , "speech" .= s
                                    ]
    toJSON (ImageMessage u) = object [ "type" .= (3 :: Int)
                                     , "imageUrl" .= u
                                     ]
    toJSON (CardMessage t s bs) = object [ "type" .= (1 :: Int)
                                         , "title" .= t
                                         , "subtitle" .= s
                                         , "buttons" .= bs
                                         ]
    toJSON (QuickRepliesMessage t rs) = object [ "type" .= (2 :: Int)
                                               , "title" .= t
                                               , "replies" .= rs
                                               ]
    toJSON (CustomPayloadMessage p) = object [ "type" .= (4 :: Int)
                                             , "payload" .= p
                                             ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> do
        (t :: Int) <- o .: "type"
        case t of
            0 -> TextMessage <$> o .: "speech"
            1 -> CardMessage <$> o .: "title" <*> o .: "subtitle" <*> o .: "buttons"
            2 -> QuickRepliesMessage <$> o .: "title" <*> o .: "replies"
            3 -> ImageMessage <$> o .: "imageUrl"
            4 -> CustomPayloadMessage <$> o .: "payload"
            _ -> fail $ "Unknown message type id: " <> show t
