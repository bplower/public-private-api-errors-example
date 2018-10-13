
module Backend.Domain where

import Backend.Errors

login :: (Either BackendError ())
login = Left . ErrorAuth $ AccountDisabled

queryDb :: (Either BackendError ())
queryDb = Left . ErrorDb $ FailedConstraint
