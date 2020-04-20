{-|
  Unique ID generator
-}
module Lib.NodeManager
  (
    NodeManager
  , generateIdx
  , runNodeManager
  )
    where

import           Control.Monad.State.Strict
import           Protolude                  hiding (State, evalState)


newtype ID = ID Text deriving (Show, Eq, Ord)

-- | NodeManager = state :: s
-- s :: Int32, a :: a
type NodeManager = State Int32

-- | Places the id into the value channel then
-- update state for the next time
generateIdx :: NodeManager Int32
generateIdx = do
  n <- get   -- dump state into what will be the value channel
  put (n+1)  -- update state
  return n        -- return a ref to the new monad

-- | Releases the monad value
runNodeManager :: NodeManager a -> a
runNodeManager m = evalState m iniManager

-- | internal
iniManager :: Int32
iniManager = 0
