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


httpsManagerWithConfig :: (MonadIO m) => (Manager -> Manager) -> m Manager
httpsManagerWithConfig = (<$> liftIO (newManager tlsManagerSettings))

httpsManager :: (MonadIO m) => m Manager
httpsManager = httpsManagerWithConfig id

userAgentHeader :: Header
userAgentHeader = ("User-Agent", "Network.PwnChk.Request.Client")

apiRequestHeaders :: Request -> Request
apiRequestHeaders req =
  let headers = userAgentHeader : requestHeaders req
  in req { requestHeaders = headers }

pwnchkRequest :: Request -> Request
pwnchkRequest req = (apiRequestHeaders req) { method = "GET" }

decode :: (Aeson.FromJSON a, MonadThrow m) => LBS.ByteString -> m a
decode body =
  case Aeson.eitherDecode' body of
    Left err -> throwM (InvalidResponseException err)
    Right val -> return val

get :: (MonadIO m, MonadThrow m) => String -> (LBS.ByteString -> m a) -> m a
get target parseBody = do
  mgr  <- httpsManager
  req  <- pwnchkRequest <$> liftIO (parseRequest target)
  resp <- liftIO $ httpLbs req mgr
  parseBody $ responseBody resp

getJSON :: (MonadIO m, MonadThrow m, Aeson.FromJSON a) => String -> m a
getJSON = flip get decode

truncatedBreaches :: (MonadIO m, MonadThrow m) => String -> m [TruncatedAccountBreach]
truncatedBreaches account =
  let addr = printf "https://haveibeenpwned.com/api/v2/breachedaccount/%s?truncateResponse=true" account
  in getJSON addr

allBreaches :: (MonadIO m, MonadThrow m) => String -> m [AccountBreachInfo]
allBreaches account =
  let addr = printf "https://haveibeenpwned.com/api/v2/breachedaccount/%s" account
  in getJSON addr

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
