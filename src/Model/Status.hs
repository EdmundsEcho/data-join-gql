
module Model.Status
  where

-- |
-- A phantom type used to track the status of a request
-- Three states of a request once it has been received.
data Status
     = Inprocess  -- promise ~ unresolved
     | Success
     | Failure
