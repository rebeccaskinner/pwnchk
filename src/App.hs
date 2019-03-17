module App where

import Network.PwnChk.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class


data AppException = RequestException PwnChkException
                  | ArgumentException String
                  deriving (Eq, Show)

instance Exception AppException

data AppCfg = AppCfg

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
getConfig = return AppCfg
