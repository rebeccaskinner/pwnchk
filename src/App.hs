{-# LANGUAGE TupleSections #-}
module App where

import System.Environment
import Network.PwnChk.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import qualified Data.Map.Strict as Map

data Arg = Option String | Flag deriving (Eq, Show)
type ArgMap = Map.Map String Arg

argMap :: [String] -> Either AppException ArgMap
argMap args =
  Map.fromList <$> mapM (\arg -> (arg, ) <$> mkArg arg) args
  where
    mkArg :: String -> Either AppException Arg
    mkArg arg =
      case splitOn '=' arg of
        [] -> Left $ ArgumentException "missing argument"
        [flag] -> Right $ Flag
        [opt,val] -> Right $ Option val
        invalid -> Left $ ArgumentException ("invalid option: " ++ arg)
    splitOn :: Char -> String -> [String]
    splitOn c s = reverse . map reverse $ splitOn' "" [] c s
    splitOn' :: String -> [String] -> Char -> String -> [String]
    splitOn' wCarry carry brk [] = wCarry : carry
    splitOn' wCarry carry c (s:esses)
      | c == s = splitOn' "" (wCarry : carry) c esses
      | otherwise = splitOn' (s:wCarry) carry c esses

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
    ("account":args) -> cfgAccount args
    ("password":args) -> cfgPassword args
    (unknown:_) -> throwM . ArgumentException $ "unknown mode: " ++ unknown

cfgAccount :: [String] -> IO AppCfg
cfgAccount args = do
  args' <- eitherToException $ argMap args


cfgPassword :: [String] -> IO AppCfg
cfgPassword args = undefined
