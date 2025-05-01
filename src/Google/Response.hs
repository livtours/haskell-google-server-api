{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Response

Define data types to represent all of the responses that are received from the Google API.
-}
module Google.Response (
    Token (..),
    Account (..),
    DateTime (..),
    Bytes (..),
    ZonedDateTime (..),
    CalendarEvent (..),
    CalendarEventList (..),
    GmailSend (..),
    GmailList (..),
    GmailMessage (..),
    MessagePart (..),
    MessagePartHeader (..),
    ExtendedProperties (..),
    FileResource (..),
    FileList (..),
    MediaContent (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withObject, withText, (.:?))
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (Options (..), defaultOptions, deriveFromJSON, deriveJSON)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, splitOn, unpack)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm, ToForm)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..), parseUrlPieces, toUrlPieces)

import Google.Type (FileId, MediaContent (..), MediaType)

{- | Binary data.

This data is passed to/from the serialisation routines as-is, and any
particular encoding or decoding (say, base64) is left up to the caller.
-}
newtype Bytes = Bytes {unBytes :: ByteString}
    deriving (Eq, Show, Read, Ord, Typeable, Generic)

instance ToHttpApiData Bytes where
    toQueryParam = TE.decodeUtf8 . unBytes
    toHeader = unBytes

instance FromHttpApiData Bytes where
    parseQueryParam = pure . Bytes . TE.encodeUtf8
    parseHeader = pure . Bytes

instance FromJSON Bytes where parseJSON = parseJSONText "Bytes"
instance ToJSON Bytes where toJSON = toJSONText

parseJSONText :: (FromHttpApiData a) => String -> Value -> Parser a
parseJSONText n = withText n (either (fail . f) pure . parseQueryParam)
  where
    f x = n <> " - " <> unpack x

toJSONText :: (ToHttpApiData a) => a -> Value
toJSONText = String . toQueryParam

newtype Textual a = Textual a
    deriving (Eq, Ord, Read, Show, Num, Fractional, Typeable, ToHttpApiData, FromHttpApiData)

instance (FromJSON a, FromHttpApiData a) => FromJSON (Textual a) where
    parseJSON (String s) =
        either (fail . unpack) (pure . Textual) (parseQueryParam s)
    parseJSON o = Textual <$> parseJSON o

instance (ToHttpApiData a) => ToJSON (Textual a) where
    toJSON (Textual x) = String (toQueryParam x)

data Token = Token
    { accessToken :: Text
    , tokenType :: Text
    , expiresIn :: Int
    }
    deriving (Eq, Generic, Show, Typeable)

deriveJSON (defaultOptions{fieldLabelModifier = snakeCase}) ''Token

instance FromForm Token

instance ToForm Token

newtype Account = Account
    { email :: Text
    }
    deriving (Eq, Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''Account

instance FromHttpApiData [Account] where
    parseUrlPiece = parseUrlPieces . (splitOn ",")

instance ToHttpApiData [Account] where
    toUrlPiece = (intercalate ",") . toUrlPieces

newtype DateTime = DateTime
    { dateTime :: UTCTime
    }
    deriving (Eq, Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''DateTime

newtype ZonedDateTime = ZonedDateTime
    { dateTime :: Maybe ZonedTime
    }
    deriving (Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''ZonedDateTime

instance Eq ZonedDateTime where
    (==) =
        ( \x y ->
            let
                toUTC :: ZonedDateTime -> Maybe UTCTime
                toUTC (ZonedDateTime z) = fmap zonedTimeToUTC z
             in
                (toUTC x) == (toUTC y)
        )

data ExtendedProperties = ExtendedProperties
    { private :: HashMap Text Text
    , shared :: HashMap Text Text
    }
    deriving (Eq, Generic, Show, Typeable)

instance FromJSON ExtendedProperties where
    parseJSON = withObject "ExtendedProperties" $ \v ->
        ExtendedProperties
            <$> (fromMaybe HashMap.empty <$> v .:? "private")
            <*> (fromMaybe HashMap.empty <$> v .:? "shared")

data CalendarEvent = CalendarEvent
    { status :: Text
    , organizer :: Account
    , creator :: Account
    , attendees :: Maybe [Account]
    , summary :: Maybe Text
    , description :: Maybe Text
    , start :: Maybe ZonedDateTime
    , end :: Maybe ZonedDateTime
    , extendedProperties :: Maybe ExtendedProperties
    }
    deriving (Eq, Generic, Show, Typeable)

deriveFromJSON defaultOptions ''CalendarEvent

data CalendarEventList = CalendarEventList
    { kind :: Text
    , summary :: Text
    , items :: [CalendarEvent]
    }
    deriving (Eq, Generic, Show, Typeable)

deriveFromJSON defaultOptions ''CalendarEventList

data GmailSend = GmailSend
    { id :: Text
    }
    deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''GmailSend

instance FromForm GmailSend

instance ToForm GmailSend

data MessagePartHeader = MessagePartHeader
    { value :: Maybe Text
    , name :: Maybe Text
    }
    deriving (Eq, Generic, Show, Typeable)
deriveJSON defaultOptions ''MessagePartHeader

data MessagePartBody = MessagePartBody
    { _mpbSize :: Maybe (Textual Int32)
    , _mpbData :: Maybe Bytes
    , _mpbAttachmentId :: Maybe Text
    }
    deriving (Eq, Generic, Show, Typeable)
deriveJSON (defaultOptions{fieldLabelModifier = snakeCase . drop 3}) ''MessagePartBody

data MessagePart = MessagePart
    { parts :: Maybe [MessagePart]
    , body :: Maybe MessagePartBody
    , mimeType :: Maybe Text
    , headers :: Maybe [MessagePartHeader]
    , partId :: Maybe Text
    , filename :: Maybe Text
    }
    deriving (Eq, Generic, Show, Typeable)
deriveJSON defaultOptions ''MessagePart

data GmailMessage = GmailMessage
    { id :: Text
    , threadId :: Text
    , labelIds :: Maybe [Text]
    , snippet :: Maybe Text
    , raw :: Maybe Bytes
    , payload :: Maybe MessagePart
    }
    deriving (Eq, Generic, Show, Typeable)
deriveJSON defaultOptions ''GmailMessage

data GmailList = GmailList
    { messages :: [GmailMessage]
    }
    deriving (Eq, Generic, Show, Typeable)
deriveJSON defaultOptions ''GmailList

data FileResource = FileResource
    { kind :: Text
    , id :: FileId
    , name :: Text
    , mimeType :: MediaType
    }
    deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''FileResource

data FileList = FileList
    { kind :: Text
    , files :: [FileResource]
    }
    deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''FileList
