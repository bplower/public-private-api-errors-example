
import qualified Shared.Errors as SE
import Backend.Errors
import Backend.Domain ( login, queryDb )

-- | Here out error is already a PublicError
apiReturnPublicError :: (Show a, SE.ToAppError a) => a -> IO ()
apiReturnPublicError = print . SE.toAppError

throwAppError :: BackendError -> IO ()
throwAppError e =
  -- apiReturnPublicError $ case e of
  --   ErrorAuth e -> (SE.toPublic e :: SE.AuthError)
  --   ErrorDb e   -> (SE.toPublic e :: SE.DbError)
  case e of
    ErrorAuth e -> apiReturnPublicError (SE.toPublic e :: SE.AuthError)
    ErrorDb e   -> apiReturnPublicError (SE.toPublic e :: SE.DbError)

-- | These represent API endpoint handlers
apiLogin :: IO ()
apiLogin = case login of
  Left e  -> throwAppError e
  Right _ -> return ()

apiQuery :: IO ()
apiQuery = case queryDb of
  Left e -> throwAppError e
  Right _ -> return ()

-- | Call the "API handlers" pretending we're recieving HTTP requests
main :: IO ()
main = do
  apiLogin
  apiQuery
