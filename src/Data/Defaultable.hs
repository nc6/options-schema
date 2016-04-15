-- |
-- Copyright : (C) 2014 Seagate Technology Limited.
-- License   : BSD3
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Defaultable where

import Control.Applicative
  ( Alternative
  , (<|>)
  )
import Data.Binary
import Data.Hashable
import Data.Typeable (Typeable)

import GHC.Generics (Generic)

data Defaultable a =
    Configured !a
  | Default !a
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Functor Defaultable where
  fmap f (Default a) = Default $ f a
  fmap f (Configured a) = Configured $ f a

instance Binary a => Binary (Defaultable a)
instance Hashable a => Hashable (Defaultable a)

fromDefault :: Defaultable a -> a
fromDefault (Configured a) = a
fromDefault (Default a) = a

-- | Make an argument optional with a delineated default.
defaultable :: Alternative f
            => a -- ^ Default value.
            -> f a -- ^ Existing option.
            -> f (Defaultable a)
defaultable a s = Configured <$> s <|> pure (Default a)
