{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module     : Lib.EitherM
-- Description: Either with a Warning capacity
--
--
module Lib.EitherM
  where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Data.Set  as Set
---------------------------------------------------------------------------------
  --
-- ** EitherM
--
type Message = Text

data EitherM a
  = RightM a
  | LeftM  ([Message], Maybe a)
  deriving (Show, Eq, Functor)

fromEitherM :: EitherM a -> Maybe a
fromEitherM (RightM a)           = Just a
fromEitherM (LeftM (_, Just a))  = Just a
fromEitherM (LeftM (_, Nothing)) = Nothing

-- |
-- Extract intersection result
--
toSet :: Ord a => EitherM a -> Set a
toSet (RightM v)           = Set.fromList [v]
toSet (LeftM (_, Just v))  = Set.fromList [v]
toSet (LeftM (_, Nothing)) = Set.fromList []

---------------------------------------------------------------------------------
  --
instance Applicative EitherM where
  pure                                          = RightM

  RightM f              <*> RightM a              = RightM $ f a

  LeftM (mss, Just f)   <*> RightM a              = LeftM (mss, Just $ f a)
  LeftM (mss, Nothing)  <*> RightM _              = LeftM (mss, Nothing)

  RightM f              <*> LeftM (mss, Just a)   = LeftM (mss, Just $ f a)
  RightM _              <*> LeftM (mss, Nothing)  = LeftM (mss, Nothing)

  LeftM (mss1, Just f)  <*> LeftM (mss2, Just a)  = LeftM (mss1 <> mss2, Just $ f a)
  LeftM (mss1, Just _)  <*> LeftM (mss2, Nothing) = LeftM (mss1 <> mss2, Nothing)
  LeftM (mss1, Nothing) <*> LeftM (mss2, Just _)  = LeftM (mss1 <> mss2, Nothing)
  LeftM (mss1, Nothing) <*> LeftM (mss2, Nothing) = LeftM (mss1 <> mss2, Nothing)



---------------------------------------------------------------------------------
  --
