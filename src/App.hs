{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module App where

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

data Arg = Option String | Flag deriving (Eq, Show)
type ArgMap = Map.Map String Arg

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

class FromArg a where
  fromArg :: String -> Either AppException a

getOpt :: FromArg a => String -> ArgMap -> Either AppException a
getOpt k m =
  case Map.lookup k m of
    Nothing -> Left $ ArgumentException $ "expected an argument: " ++ k ++ " in " ++ show m
    Just (Option a) -> fromArg a
    Just Flag -> Left $ ArgumentException $ "expected an argument but got a flag: " ++ k

getFlag :: String -> ArgMap -> Either AppException Bool
getFlag k m =
  case Map.lookup k m of
    Nothing -> Right False
    Just Flag -> Right True
    Just _ -> Left $ ArgumentException $ "expected a flag but got an argument: " ++ k

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


data AppException = RequestException PwnChkException
                  | ArgumentException String
                  deriving (Eq, Show)

instance Exception AppException

data AppCfg = AccountCfg { _acctAccount :: String
                         , _acctVerbose :: Bool }
            | PasswordCfg { _pwPassword :: String }
            | HelpCfg

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

runAppT :: AppCfg -> AppT a -> IO a
runAppT cfg act =
  handleErr $ runReaderT (runExceptT . unAppT $ act) cfg
  where
    handleErr :: IO (Either AppException a) -> IO a
    handleErr v = v >>= \case
      Left err -> ioError $ userError (show err)
      Right a -> return a

cfgAndRun :: AppT a -> IO a
cfgAndRun = (getConfig >>=) . flip runAppT

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

checkForHelp :: [String] -> (ArgMap -> IO AppCfg) -> IO AppCfg
checkForHelp args f =
  if any (== "--help") args
  then return HelpCfg
  else (eitherToException id $ argMap args) >>= f

cfgAccount :: ArgMap -> IO AppCfg
cfgAccount args =
  eitherToException id $ AccountCfg
  <$> "--account" `getOpt` args
  <*> "--verbose" `getFlag` args

cfgPassword :: ArgMap -> IO AppCfg
cfgPassword args =
    let passwordMethods = catMaybes $ map (\k -> (k,) <$> Map.lookup k args ) ["--hash","--unsafe-password"]
    in case passwordMethods of
         [] -> (PasswordCfg . hashPassword) <$> promptForPassword
         [("--hash",Option sha)] -> return $ PasswordCfg sha
         [("--unsafe-password", Option pw)] -> return $ PasswordCfg (hashPassword pw)
         _ -> throwM $ ArgumentException "expected 0 or 1 of --hash, --unsafe-password"
  where
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
