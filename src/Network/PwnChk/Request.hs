-- | This module implements the Have I Been Pwned API
module Network.PwnChk.Request where

import Network.PwnChk.Exception
import Network.PwnChk.Types

import Control.Monad.IO.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Monad.Catch
import Data.List
import Text.Printf
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


-- | httpsManagerWithConfig takes a config transform function and
-- | returns an https connection manager with the modified config
httpsManagerWithConfig :: (MonadIO m) => (Manager -> Manager) -> m Manager
httpsManagerWithConfig = (<$> liftIO (newManager tlsManagerSettings))

-- | httpsManager returns a default https manager
httpsManager :: (MonadIO m) => m Manager
httpsManager = httpsManagerWithConfig id

-- | userAgentHeader defines the user agent used for these API
-- | requests
userAgentHeader :: Header
userAgentHeader = ("User-Agent", "Network.PwnChk.Request.Client")

-- | apiRequestHeaders takes an HTTP request and adds all of the
-- | necessary headers
apiRequestHeaders :: Request -> Request
apiRequestHeaders req =
  let headers = userAgentHeader : requestHeaders req
  in req { requestHeaders = headers }

-- | pwnchkRequest generates an an API request
pwnchkRequest :: Request -> Request
pwnchkRequest req = (apiRequestHeaders req) { method = "GET" }

-- | decode is a utility function that decodes a response value and
-- | throws an exception if parsing fails.
decode :: (Aeson.FromJSON a, MonadThrow m) => LBS.ByteString -> m a
decode body =
  case Aeson.eitherDecode' body of
    Left err -> throwM (InvalidResponseException err)
    Right val -> return val

-- | performs a GET request to the given URL and passes the body on to
-- | a monadic transformation function
get :: (MonadIO m, MonadThrow m) => String -> (LBS.ByteString -> m a) -> m a
get target parseBody = do
  mgr  <- httpsManager
  req  <- pwnchkRequest <$> liftIO (parseRequest target)
  resp <- liftIO $ httpLbs req mgr
  parseBody $ responseBody resp

-- | convenience function get get a JSON encoded body
getJSON :: (MonadIO m, MonadThrow m, Aeson.FromJSON a) => String -> m a
getJSON = flip get decode

-- | get a list of all truncated breaches for a given account
truncatedBreaches :: (MonadIO m, MonadThrow m) => String -> m [TruncatedAccountBreach]
truncatedBreaches account =
  let addr = printf "https://haveibeenpwned.com/api/v2/breachedaccount/%s?truncateResponse=true" account
  in getJSON addr

-- | get a full list of breach data for a given account
allBreaches :: (MonadIO m, MonadThrow m) => String -> m [AccountBreachInfo]
allBreaches account =
  let addr = printf "https://haveibeenpwned.com/api/v2/breachedaccount/%s" account
  in getJSON addr

-- | get a count of the number of times a password has been compromised
passwords :: (MonadIO m, MonadThrow m) => String -> m Integer
passwords pwSha =
  let pfx = take 5 pwSha
      sfx = drop 5 pwSha
      addr = printf "https://api.pwnedpasswords.com/range/%s" pfx
  in do
    allMatches <- get addr parsePasswordBreaches
    return $ fromMaybe 0 $ (passwordCount <$> findJust (pwMatch sfx) allMatches)
    where
      findJust :: (a -> Bool) -> [a] -> Maybe a
      findJust f = listToMaybe . filter f
      pwMatch :: String -> PasswordBreach -> Bool
      pwMatch s = (== s) . T.unpack . passwordHash
