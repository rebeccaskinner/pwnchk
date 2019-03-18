{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module App ( cfgAndRun
           , AppCfg (..)
           , module Control.Monad.Reader
           , module Control.Monad.IO.Class
           ) where

import System.Environment
import Network.PwnChk.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import Data.Char
import Data.Maybe
import System.IO
import Crypto.Hash.SHA1
import Numeric
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

-- | Arg defines a single command line argument, which may be an option
-- | (a value-containing arg ument) or a flag (a non-value containing
-- | argument)
data Arg = Option String | Flag deriving (Eq, Show)

-- | ArgMap defines the set of arguments passed in on the command line
type ArgMap = Map.Map String Arg

-- | argMap takes a list of command line arguments and generates an
-- | arg map.  It will fail if the arguments are malformed
argMap :: [String] -> Either AppException ArgMap
argMap args =
  Map.fromList <$> mapM (\arg -> mkArg arg) args
  where
    mkArg :: String -> Either AppException (String, Arg)
    mkArg arg =
      case splitOn '=' arg of
        [] -> Left $ ArgumentException "missing argument"
        [flag] -> Right $ (flag, Flag)
        [opt,val] -> Right $ (opt, Option val)
        invalid -> Left $ ArgumentException ("invalid option: " ++ arg)
    splitOn :: Char -> String -> [String]
    splitOn c s = reverse . map reverse $ splitOn' "" [] c s
    splitOn' :: String -> [String] -> Char -> String -> [String]
    splitOn' wCarry carry brk [] = wCarry : carry
    splitOn' wCarry carry c (s:esses)
      | c == s = splitOn' "" (wCarry : carry) c esses
      | otherwise = splitOn' (s:wCarry) carry c esses

-- | FromArg is a convenience class to help us manage argument parsing
class FromArg a where
  fromArg :: String -> Either AppException a

-- | Get a single option from the argument map.  It will fail if the
-- | argument hasn't been set, or if it was passed in as a flag
getOpt :: FromArg a => String -> ArgMap -> Either AppException a
getOpt k m =
  case Map.lookup k m of
    Nothing -> Left $ ArgumentException $ "expected an argument: " ++ k ++ " in " ++ show m
    Just (Option a) -> fromArg a
    Just Flag -> Left $ ArgumentException $ "expected an argument but got a flag: " ++ k

-- | Get a flag.  If the flag was not set, it will return false, if
-- | the flag was set, returns true.  If the argument was passed in as
-- | an option it will fail.
getFlag :: String -> ArgMap -> Either AppException Bool
getFlag k m =
  case Map.lookup k m of
    Nothing -> Right False
    Just Flag -> Right True
    Just _ -> Left $ ArgumentException $ "expected a flag but got an argument: " ++ k

{- FromArg instances -}

instance {-# OVERLAPS #-} FromArg String where fromArg = Right
instance {-# OVERLAPS #-} FromArg Bool where
  fromArg s =
    case map toLower s of
      "true" -> Right True
      "false" -> Right False
      invalid -> Left $ ArgumentException $ "Non-truthy value: " ++ invalid
instance {-# OVERLAPPABLE #-} Read r => FromArg r where
  fromArg s = case read s of
    Left err -> Left $ ArgumentException err
    Right a -> Right a

-- | AppException defines some basic exception types for our
-- | application.  In this case, we're only defining two, a
-- | RequestException that we get from the PwnChk code, or an
-- | ArgumentException that we generate while parsing command line
-- | arguments.
data AppException = RequestException PwnChkException
                  | ArgumentException String
                  deriving (Eq, Show)

-- use the default Exception instance
instance Exception AppException

-- | AppCfg defines the runtime configuration and modes for the
-- | application.
data AppCfg = AccountCfg { _acctAccount :: String
                         , _acctVerbose :: Bool }
            | PasswordCfg { _pwPassword :: String }
            | HelpCfg

-- | AppT is the application monad.
newtype AppT a = AppT
  { unAppT :: ExceptT AppException (ReaderT AppCfg IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader AppCfg
             , MonadError AppException
             , MonadThrow
             )

-- | Run an application action with a given config, resulting in an IO
-- | action.
runAppT :: AppCfg -> AppT a -> IO a
runAppT cfg act =
  handleErr $ runReaderT (runExceptT . unAppT $ act) cfg
  where
    handleErr :: IO (Either AppException a) -> IO a
    handleErr v = v >>= \case
      Left err -> ioError $ userError (show err)
      Right a -> return a

-- | generates a config from the command line and runs an AppT action
-- | with it.
cfgAndRun :: AppT a -> IO a
cfgAndRun = (getConfig >>=) . flip runAppT

-- | getConfig looks at the command line arguments and generates a
-- | program config
getConfig :: IO AppCfg
getConfig = do
  args <- getArgs
  when (null args) $
    throwM $ ArgumentException "missing argument"
  case args of
    ("help":_) -> return $ HelpCfg
    ("account":args) -> checkForHelp args cfgAccount
    ("password":args) -> checkForHelp args cfgPassword
    (unknown:_) -> throwM . ArgumentException $ "unknown mode: " ++ unknown

-- | Checks for a help flag in a list of arguments
checkForHelp :: [String] -> (ArgMap -> IO AppCfg) -> IO AppCfg
checkForHelp args f =
  if any (== "--help") args
  then return HelpCfg
  else (eitherToException id $ argMap args) >>= f

-- | cfgAccount configures the options for the account mode
cfgAccount :: ArgMap -> IO AppCfg
cfgAccount args =
  eitherToException id $ AccountCfg
  <$> "--account" `getOpt` args
  <*> "--verbose" `getFlag` args

-- | cfgPassword gets the configuration data for the password mode.
-- | More specifically, it will either use the provided hash, has the
-- | provided password, or prompt for a password.
cfgPassword :: ArgMap -> IO AppCfg
cfgPassword args =
    let passwordMethods = catMaybes $ map (\k -> (k,) <$> Map.lookup k args ) ["--hash","--unsafe-password"]
    in case passwordMethods of
         [] -> (PasswordCfg . hashPassword) <$> promptForPassword
         [("--hash",Option sha)] -> return $ PasswordCfg sha
         [("--unsafe-password", Option pw)] -> return $ PasswordCfg (hashPassword pw)
         _ -> throwM $ ArgumentException "expected 0 or 1 of --hash, --unsafe-password"
  where
    -- | promptForPassword will manually disable terminal echoing
    -- | before prompting for a password.  NB: this is relying on the
    -- | garbage collector to handle freeing the string holding the
    -- | password after we've hashed it.  It would be more secure here
    -- | to use a native mutable string that we can overwrite with
    -- | garbage data and free as part of the function call (or call
    -- | out to a mature crypto library providing a safe function).
    promptForPassword :: IO String
    promptForPassword = do
      doEcho <- hGetEcho stdin
      hSetEcho stdin False
      putStrLn "Password:"
      pw <- hGetLine stdin
      hSetEcho stdin doEcho
      putStrLn ""
      return pw
    hashPassword :: String -> String
    hashPassword =
      concatMap (flip showHex "") . BS.unpack . hash . BC.pack
