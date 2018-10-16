{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.Errors where

import qualified Shared.Errors as SE

data BackendError
  = ErrorAuth AuthError
  | ErrorDb DbError
  deriving ( Show )

instance SE.ToAppError BackendError where
  toAppError (ErrorAuth e) = SE.toAppAuthError e
  toAppError (ErrorDb e)   = SE.toAppDbError e

-- Errors for authentication

data AuthError
  = IncorrectPassword
  | UsernameNotFound
  | AccountDisabled
  | SessionInvalid
  | AuthServiceFailure
  deriving ( Show )

instance SE.PublicError AuthError SE.AuthError where
  toPublic AuthServiceFailure = SE.AuthServiceFailure SE.ServiceUnavailable
  toPublic SessionInvalid     = SE.SessionInvalid
  toPublic _                  = SE.BadCredentials

-- Errors for database interaction

data DbError
  = FailedConstraint
  | NotFound
  | UnhandledSQLError String
  | DbServiceFailure
  deriving ( Show )

instance SE.PublicError DbError SE.DbError where
  toPublic DbServiceFailure = SE.DbServiceFailure SE.ServiceUnavailable
  toPublic NotFound         = SE.NotFound
  toPublic _                = SE.QueryFailure
