module Network.PwnChk.Exception where

import Control.Monad.Catch

data PwnChkException = InvalidStatusException Int
                     | InvalidResponseException String
                     | UnknownError String deriving (Eq, Show)

instance Exception PwnChkException

eitherToException :: (MonadThrow m) => (a -> PwnChkException) -> Either a b -> m b
eitherToException f =
  \case
    Left e -> throwM $ f e
    Right v -> return v
