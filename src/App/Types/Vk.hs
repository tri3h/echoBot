{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Types.Vk where

import Data.Aeson (FromJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (FromJSON (parseJSON), Parser)
import qualified Data.ByteString as BS
import qualified Data.Text as Text

newtype GroupID = GroupID BS.ByteString

newtype Host = Host BS.ByteString

data Message = Message
  { tsMes :: String,
    peerID :: Integer,
    text :: Maybe String,
    forward :: Maybe [ForwardID],
    geo :: Maybe GeoInfo,
    attachments :: Maybe [Attachment]
  }
  deriving (Show)

data Attachment
  = Media
      { name :: String,
        ownerID :: Integer,
        mediaID :: Integer,
        accessKey :: Maybe String
      }
  | Sticker {stickerID :: Integer}
  deriving (Show)

data ConnectionInfo = ConnectionInfo
  { key :: String,
    server :: String,
    ts :: String
  }
  deriving (Show)

data GeoInfo = GeoInfo
  { lat :: Float,
    long :: Float
  }
  deriving (Show)

newtype ForwardID = ForwardID {forwardID :: Integer} deriving (Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    tsMes <- o .: "ts"
    [updates] <- o .: "updates"
    object <- updates .: "object"
    message <- object .: "message"
    peerID <- message .: "peer_id"
    forward <- message .:? "fwd_messages"
    geo <- message .:? "geo"
    text <- message .:? "text"
    attachments <- message .:? "attachments"
    return Message {..}

instance FromJSON ForwardID where
  parseJSON = withObject "ForwardID" $ \o -> do
    id' <- o .: "id"
    return $ ForwardID id'

instance FromJSON GeoInfo where
  parseJSON = withObject "GeoInfo" $ \o -> do
    coord <- o .: "coordinates"
    lat <- coord .: "latitude"
    long <- coord .: "longitude"
    return $ GeoInfo {..}

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o -> do
    name <- o .: "type"
    media <- o .: Text.pack name
    case name of
      "sticker" -> do
        stickerID <- media .: "sticker_id"
        return Sticker {..}
      _ -> do
        id' <- media .:? "owner_id" :: Parser (Maybe Integer)
        ownerID <- case id' of
          Just _ -> media .: "owner_id"
          Nothing -> media .: "to_id"
        mediaID <- media .: "id"
        accessKey <- media .:? "access_key"
        return Media {..}

instance FromJSON ConnectionInfo where
  parseJSON = withObject "ConnectionInfo" $ \obj -> do
    response <- obj .: "response"
    key <- response .: "key"
    server <- response .: "server"
    ts <- response .: "ts"
    return ConnectionInfo {..}
