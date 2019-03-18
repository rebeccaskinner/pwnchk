-- | This module provides the types used by Network.PwnChk.Request to
-- | make API requests
module Network.PwnChk.Types where

import Network.PwnChk.Exception

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Time.Format
import Control.Monad.Catch
import Text.Read
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | TruncatedAccountBreach is the response type for a truncated
-- | account response
data TruncatedAccountBreach = TruncatedAccountBreach
  { _truncatedAccountResponseName :: T.Text } deriving (Eq, Show)

instance FromJSON TruncatedAccountBreach where
  parseJSON (Object v) = TruncatedAccountBreach <$> v .: "Name"
  parseJSON invalid = typeMismatch "TruncatedAccountBreach" invalid


type DataClass = T.Text

-- | AccountBreachInfo is the response type for a full account breach
-- | query response
data AccountBreachInfo = AccountBreachInfo
  { _acctResponseName         :: T.Text
  , _acctResponseTitle        :: T.Text
  , _acctResponseDomain       :: T.Text
  , _acctResopnseBreachDate   :: UTCTime
  , _acctResponseAddedDate    :: UTCTime
  , _acctResponseModifiedDate :: UTCTime
  , _acctResponsePwnCount     :: Integer
  , _acctResponseDescription  :: T.Text
  , _acctResponseClasses      :: [DataClass]
  , _acctResponseIsVerified   :: Bool
  , _acctResponseIsFabricated :: Bool
  , _acctResponseIsSensitive  :: Bool
  , _acctResponseIsRetired    :: Bool
  , _acctResponseIsSpamList   :: Bool
  } deriving (Eq, Show)

instance FromJSON AccountBreachInfo where
  parseJSON (Object v) = AccountBreachInfo
    <$> v .: "Name"
    <*> v .: "Title"
    <*> v .: "Domain"
    <*> (getTime =<< v .: "BreachDate")
    <*> (getTime =<< v .: "AddedDate")
    <*> (getTime =<< v .: "ModifiedDate")
    <*> v .: "PwnCount"
    <*> v .: "Description"
    <*> v .: "DataClasses"
    <*> v .: "IsVerified"
    <*> v .: "IsFabricated"
    <*> v .: "IsSensitive"
    <*> v .: "IsRetired"
    <*> v .: "IsSpamList"

-- | getTime is a utility function that attempts to parse a timestamp
-- | first as an ISO 8601 format string, and, if that fails, as a
-- | YYYY-MM-DD string
getTime :: T.Text -> Parser UTCTime
getTime s =
  let s' = T.unpack s
  in do
  -- NB: parseISO8601 does not support YYYY-MM-DD format dates, so we
  -- need to catch failure cases and explicitly try to reparse them
  -- before failing.
  case parseISO8601 s' of
    Nothing ->
      parseTimeM True defaultTimeLocale "%F" s'
    Just t -> return t

-- | PasswordBreach represents the response value of a password breach
-- | query
data PasswordBreach = PasswordBreach
  { passwordHash  :: T.Text
  , passwordCount :: Integer
  } deriving (Eq, Show)

parsePasswordBreach' :: (MonadThrow m) => T.Text -> m PasswordBreach
parsePasswordBreach' t =
  case T.splitOn ":" t of
    [hash, cnt] ->
      let cnt' = readEither . T.unpack $ cnt
      in PasswordBreach hash <$> eitherToException InvalidResponseException cnt'

-- | parsePasswordBreaches parses a text password into a a set of breach responses
parsePasswordBreaches' :: (MonadThrow m) => T.Text -> m [PasswordBreach]
parsePasswordBreaches' = mapM parsePasswordBreach' . T.lines

-- | Like parsePasswordBreaches' but with a bytestring instead of a text
parsePasswordBreaches :: MonadThrow m => BL.ByteString -> m [PasswordBreach]
parsePasswordBreaches = parsePasswordBreaches' . decodeUtf8 . BL.toStrict
