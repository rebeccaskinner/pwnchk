module Network.PwnChk.Exception where

import Control.Monad.Catch

-- | PwnChkExcepttion defines a few different exceptions that we could
-- | encounter while querying the API
data PwnChkException =
  -- | InvalidStatusException is returned when we get an unexpected HTTP
  -- | status code
  InvalidStatusException Int
  -- | InvalidResponseException is returned when the response we
  -- | receive cannot be parsed as expected
  | InvalidResponseException String
  -- | UnknownError is returned for all other errors
  | UnknownError String deriving (Eq, Show)

-- Define a default Exception instance for PwnChkException
instance Exception PwnChkException

-- | eitherToException is a utility function to generate exception
-- | values from Either values
eitherToException :: (MonadThrow m, Exception e) => (a -> e) -> Either a b -> m b
eitherToException f =
  \case
    Left e -> throwM $ f e
    Right v -> return v
