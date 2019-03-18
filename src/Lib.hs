module Lib
    ( someFunc
    ) where
import App
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.PwnChk.Request
import Network.PwnChk.Types
import Text.Printf
import Data.Time.Clock
import Data.List
import qualified Data.Text as T

someFunc :: IO ()
someFunc = cfgAndRun $ do
  mode <- ask
  case mode of
    AccountCfg acct True -> do
      breaches <- allBreaches acct
      liftIO $ mapM_ prettyprintLongBreach breaches
    AccountCfg acct False -> do
      breaches <- truncatedBreaches acct
      liftIO $ mapM_ prettyprintShortBreach breaches
    PasswordCfg pw -> do
      matches <- passwords pw
      liftIO . putStrLn $ printf "found %d matches" matches
    HelpCfg -> liftIO $ putStrLn helpText

prettyprintShortBreach :: TruncatedAccountBreach -> IO ()
prettyprintShortBreach (TruncatedAccountBreach name) = putStrLn . T.unpack $ name

prettyprintLongBreach :: AccountBreachInfo -> IO ()
prettyprintLongBreach a =
  putStrLn . unlines . map (showField a) $
  [ ("name:          " , T.unpack . _acctResponseName)
  , ("title:         " , T.unpack . _acctResponseTitle)
  , ("domain:        " , T.unpack . _acctResponseDomain)
  , ("breach date:   " , show .     _acctResopnseBreachDate)
  , ("added date:    " , show .     _acctResponseAddedDate)
  , ("modified date: " , show .     _acctResponseModifiedDate)
  , ("pwn count:     " , show .     _acctResponsePwnCount)
  , ("description:   " , T.unpack . _acctResponseDescription)
  , ("classes:       " , show .     _acctResponseClasses)
  , ("verified:      " , show .     _acctResponseIsVerified)
  , ("fabricated:    " , show .     _acctResponseIsFabricated)
  , ("sensitive:     " , show .     _acctResponseIsSensitive)
  , ("retired:       " , show .     _acctResponseIsRetired)
  , ("spam list:     " , show .     _acctResponseIsSpamList)
  ]
  where
    showField :: AccountBreachInfo -> (String, AccountBreachInfo -> String) -> String
    showField a (fname,f) = fname ++ (f a)

helpText :: String
helpText =
  "pwnchk: lookup known breaches of passwords or accounts\n" ++
  "usage: pwnchk <account|password> options\n\n" ++
  "OPTIONS:\n" ++
  "account options:\n" ++
  "  --account=<account> (required) set the account to look up\n" ++
  "  --verbose (optional) display more information about breaches\n" ++
  "password options:\n" ++
  "  --hash=<sha1> (optional, conflicts with --unsafe-password) sha to lookup\n" ++
  " --unsafe-password=<password> (optional, conflicts with --hash) password to lookup\n"
