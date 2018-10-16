{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Shared.Errors where

-- Root public application error type

class ToAppError a where
  toAppError :: a -> AppError

data AppError
  = ErrorAuth AuthError
  | ErrorDb DbError
  deriving (Show)

class (ToAppError b) => PublicError a b where
  toPublic :: a -> b

-- Public authentication error type
data AuthError
  = BadCredentials
  | SessionInvalid
  | AuthServiceFailure ServiceError
  deriving (Show)

instance ToAppError AuthError where
  toAppError = ErrorAuth

-- | Wraps toPublic, pinning the type we're converting to
-- This can be replicated by saying `toPublic a :: AuthError`
toPubAuthError :: (PublicError a AuthError) => a -> AuthError
toPubAuthError = toPublic

toAppAuthError :: (PublicError a AuthError) => a -> AppError
toAppAuthError = toAppError . toPubAuthError

-- Public database error type
data DbError
  = QueryFailure
  | NotFound
  | DbServiceFailure ServiceError
  deriving (Show)

toPubDbError :: (PublicError a DbError) => a -> DbError
toPubDbError = toPublic

toAppDbError :: (PublicError a DbError) => a -> AppError
toAppDbError = toAppError . toPubDbError

instance ToAppError DbError where
  toAppError = ErrorDb

-- General error types
data ServiceError
  = ServiceUnavailable
  deriving (Show)
