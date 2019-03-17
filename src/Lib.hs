module Lib
    ( someFunc
    ) where
import App
import Control.Monad.IO.Class
import Control.Monad.Reader

someFunc :: IO ()
someFunc = cfgAndRun $ do
  mode <- ask
  case mode of
    AccountCfg acct verbose -> do
      liftIO $ putStrLn "getting account info"
      liftIO $ putStrLn $ "account: " ++ acct
      liftIO $ putStrLn $ "verbose: " ++ show verbose
      return ()
    PasswordCfg pw -> do
      liftIO $ putStrLn "getting password info"
      liftIO $ putStrLn $ "sha1: " ++ show pw
      return ()
    HelpCfg -> do
      liftIO $ putStrLn "showing help info"
      return ()
