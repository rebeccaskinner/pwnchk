module Network.PwnChk.Exception where

import Control.Monad.Catch

data PwnChkException = InvalidStatusException Int
                     | InvalidResponseException String
                     | UnknownError String deriving (Eq, Show)

instance Exception PwnChkException

eitherToException :: (MonadThrow m, Exception e) => (a -> e) -> Either a b -> m b
eitherToException f =
  \case
    Left e -> throwM $ f e
    Right v -> return v
