{- | Create a signed JWT needed to make the access token request
to gain access to Google APIs for server to server applications.

For all usage details, see https://developers.google.com/identity/protocols/OAuth2ServiceAccount

This module is borrowed from google-oauth2-jwt package.
-}
module Google.JWT (
    JWT,
    HasJWT (..),
    readServiceKeyFile,
    SignedJWT (..),
    Email (..),
    Scope (..),
    getSignedJWT,
) where

import Control.Monad (unless)
import Crypto.PubKey.RSA.Types (PrivateKey)
import Data.Aeson (decode, toJSON, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Exts (IsList (fromList))
import qualified Web.JWT as JWT

class HasJWT a where
    getJwt :: a -> JWT

instance HasJWT JWT where
    getJwt :: JWT -> JWT
    getJwt = id

data JWT = JWT
    { clientEmail :: Email
    , privateKey :: PrivateKey
    }
    deriving (Eq, Show, Read)

readServiceKeyFile :: FilePath -> IO (Maybe JWT)
readServiceKeyFile fp = do
    content <- LBS.readFile fp
    return $ do
        result <- decode content
        (pkey, clientEmail) <-
            flip parseMaybe result $ \obj -> do
                pkey <- obj .: "private_key"
                clientEmail <- obj .: "client_email"
                pure (pkey, clientEmail)
        JWT (Email clientEmail) <$> JWT.readRsaSecret (encodeUtf8 pkey)

newtype SignedJWT = SignedJWT
    { unSignedJWT :: ByteString
    }
    deriving (Eq, Show, Read, Ord)

newtype Email = Email
    { unEmail :: Text
    }
    deriving (Eq, Show, Read, Ord)

data Scope
    = ScopeCalendarFull
    | ScopeCalendarRead
    | ScopeGmailFull
    | ScopeGmailSend
    | ScopeDriveFile
    | ScopeDriveMetadataRead
    | ScopeSpreadsheets
    deriving (Eq, Show, Read, Ord)

{- | Make sure if you added new scope, update configuration in page bellow.
  https://admin.google.com/uzuz.jp/AdminHome?chromeless=1#OGX:ManageOauthClients
-}
scopeUrl :: Scope -> Text
scopeUrl ScopeCalendarFull = "https://www.googleapis.com/auth/calendar"
scopeUrl ScopeCalendarRead = "https://www.googleapis.com/auth/calendar.readonly"
scopeUrl ScopeGmailSend = "https://www.googleapis.com/auth/gmail.send"
scopeUrl ScopeGmailFull = "https://www.googleapis.com/auth/gmail"
scopeUrl ScopeDriveFile = "https://www.googleapis.com/auth/drive.file"
scopeUrl ScopeDriveMetadataRead = "https://www.googleapis.com/auth/drive.metadata.readonly"
scopeUrl ScopeSpreadsheets = "https://www.googleapis.com/auth/spreadsheets"

{- | Create the signed JWT ready for transmission
in the access token request as assertion value.

>grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=
-}
getSignedJWT ::
    JWT ->
    -- | The email address of the user for which the
    -- application is requesting delegated access.
    Maybe Email ->
    -- | The list of the permissions that the application requests.
    [Scope] ->
    -- | Expiration time (maximum and default value is an hour, 3600).
    Maybe Int ->
    -- | Either an error message or a signed JWT.
    IO (Either String SignedJWT)
getSignedJWT JWT{..} msub scopes maxExpTime = do
    let xt = fromIntegral $ fromMaybe 3600 maxExpTime
    unless (xt >= 1 && xt <= 3600) (fail "Bad expiration time")
    now <- getPOSIXTime
    let expTime = now + xt

    let cs =
            mempty
                { JWT.iss = JWT.stringOrURI $ unEmail clientEmail
                , JWT.sub = JWT.stringOrURI . unEmail =<< msub
                , JWT.aud = Right . return <$> JWT.stringOrURI "https://www.googleapis.com/oauth2/v4/token"
                , JWT.iat = JWT.numericDate now
                , JWT.exp = JWT.numericDate expTime
                , JWT.unregisteredClaims =
                    JWT.ClaimsMap $
                        fromList
                            [("scope", toJSON (T.intercalate " " (map scopeUrl scopes)))]
                }

        -- JOSE header
        hdr = mempty{JWT.typ = Just "JWT", JWT.alg = Just JWT.RS256}

    -- Sign the JWT with RS256
    let jwtText = JWT.encodeSigned (JWT.EncodeRSAPrivateKey privateKey) hdr cs
    return $ Right $ SignedJWT $ encode $ encodeUtf8 jwtText
