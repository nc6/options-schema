-- |
-- Copyright : (C) 2014 Seagate Technology Limited.
-- License   : BSD3

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Options.Schema (
    Alt(..)
  , liftAlt
  , Schema
  , Name(..)
  , ArgumentDefault(..)
  , Description(..)
  , Argument(..)
  , Option(..)
  , Block(..)
) where

import Control.Monad ((>=>))
import Control.Alternative.FreeStar

type Schema a = Alt Option a

data Name =
    LongName !String
  | ShortName !Char
  deriving (Eq, Ord, Show)

data ArgumentDefault a = ArgumentDefault (Maybe a) (Maybe String)

instance Functor ArgumentDefault where
  fmap f (ArgumentDefault val pp) = ArgumentDefault (fmap f val) pp

data Description = Description {
    dSummary :: Maybe String
  , dDetail :: Maybe String
}

instance Monoid Description where
  mempty = Description Nothing Nothing
  (Description a b) `mappend` (Description c d) =
    Description (a `mergeDesc` c) (b `mergeDesc` d) where
      mergeDesc Nothing x = x
      mergeDesc x Nothing = x
      mergeDesc x _ = x

data Argument a = Argument {
    aMetavar :: Maybe String
  , aReader :: forall m. Monad m => String -> m a
  , aDefault :: ArgumentDefault a
  , aDescr :: Description
}

instance Functor Argument where
  fmap f (Argument n r def descr) =
    Argument n (r >=> return . f) def' descr where
      def' = fmap f def

data Option a = Option {
    oNames :: [Name]
  , oDescription :: Description
  , oBlock :: Block a
}

instance Functor Option where
  fmap f (Option n d b) = Option n d (fmap f b)

data Block a =
    SingleArgument (Argument a)
  | Subsection (Schema a)

instance Functor Block where
  fmap f (SingleArgument x) = SingleArgument $ fmap f x
  fmap f (Subsection x) = Subsection $ fmap f x
