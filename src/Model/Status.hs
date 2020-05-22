{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Model.Status
-- Description : Describes the status of a 'Model.Request'
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- /Note/: This concept may be applicable outside of 'Model.Request' and thus
-- will required being replicated and refactored accordingly.
--
module Model.Status
  where

-- |
-- ** Request Status
--
-- A phantom type used to track the status of a request that starts and ends as
-- follows:
--
-- * Start : instantiation of @GQL Request Input@
-- * End: some version of an instantiated "Mode.Request"
--
data Status
     = Inprocess  -- promise ~ unresolved; incomplete @gql request input -> Model@
     | Success    -- Instantiation of a Request
     | Validated  -- Structurally sound (likely redundant with instantiation)
     | Optimized  -- Without redundancy
     | Failure    -- Unused but completes the concept
     | ETL        -- Not part of the Request (identifies instantiator)



---------------------------------------------------------------------------------
