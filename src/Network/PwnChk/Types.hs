module Network.PwnChk.Types where

import Network.PwnChk.Exception

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.ISO8601
import Control.Monad.Catch
import Text.Read
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

type TruncatedAccountResponse = [TruncatedAccountBreach]

data TruncatedAccountBreach = TruncatedAccountBreach
  { _truncatedAccountResponseName :: T.Text } deriving (Eq, Show)

instance FromJSON TruncatedAccountBreach where
  parseJSON (Object v) = TruncatedAccountBreach <$> v .: "Name"
  parseJSON invalid = typeMismatch "TruncatedAccountBreach" invalid

type DataClass = T.Text

type AccountBreachResponse = [AccountBreachInfo]

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

getTime :: T.Text -> Parser UTCTime
getTime s = do
  case parseISO8601 (T.unpack s) of
    Nothing -> fail "invalid time format"
    Just t -> return t

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

parsePasswordBreaches' :: (MonadThrow m) => T.Text -> m [PasswordBreach]
parsePasswordBreaches' = mapM parsePasswordBreach' . T.lines

parsePasswordBreaches :: MonadThrow m => BL.ByteString -> m [PasswordBreach]
parsePasswordBreaches = parsePasswordBreaches' . decodeUtf8 . BL.toStrict
