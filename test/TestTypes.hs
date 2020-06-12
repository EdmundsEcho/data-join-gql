{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : TestTypes
-- Description : Types specific for the testing context
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module TestTypes
  ( module TestTypes
  , module WithAppContext
  )
  where
--------------------------------------------------------------------------------
import           Protolude
import           WithAppContext
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** AppType for testing
-- |
--
newtype TestObs a =
    TestObs
        { iniApp :: ReaderT Env (NoLoggingT IO) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadLogger
             , MonadThrow
             , MonadCatch
             )

--------------------------------------------------------------------------------
-- ** From App context to IO
-- |
--
nat :: Env -> TestObs a -> IO a
nat env app' = runNoLoggingT (
                 runReaderT (iniApp app') env)
--------------------------------------------------------------------------------
