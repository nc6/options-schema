-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Defaultable where

import Data.Typeable (Typeable)

data Defaultable a =
    Configured !a
  | Default !a
  deriving (Eq, Ord, Show, Typeable)

instance Functor Defaultable where
  fmap f (Default a) = Default $ f a
  fmap f (Configured a) = Configured $ f a

fromDefault :: Defaultable a -> a
fromDefault (Configured a) = a
fromDefault (Default a) = a