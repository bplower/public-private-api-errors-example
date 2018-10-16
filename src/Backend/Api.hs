
module Backend.Api
  ( apiLogin
  , apiQuery
  ) where

import Backend.Errors
import Backend.Domain ( login, queryDb )
import qualified Shared.Errors as SE

-- | Here out error is already a PublicError
throwAppError :: (Show a, SE.ToAppError a) => a -> IO ()
throwAppError e = do
  let e' = SE.toAppError e
  print (e, e')

-- | These represent API endpoint handlers
apiLogin :: IO ()
apiLogin = case login of
  Left e  -> throwAppError e
  Right _ -> return ()

apiQuery :: IO ()
apiQuery = case queryDb of
  Left e -> throwAppError e
  Right _ -> return ()
