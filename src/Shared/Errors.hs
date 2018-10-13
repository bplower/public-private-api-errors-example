{-# LANGUAGE MultiParamTypeClasses #-}

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
  toAppError a = ErrorAuth a

-- Public database error type
data DbError
  = QueryFailure
  | NotFound
  | DbServiceFailure ServiceError
  deriving (Show)

instance ToAppError DbError where
  toAppError a = ErrorDb a

-- General error types
data ServiceError
  = ServiceUnavailable
  deriving (Show)
