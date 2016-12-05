{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Alternative.Freer 
  ( Alt
  , runAlt
  , liftAlt
  , hoistAlt
  , retractAlt
  ) where

import Control.Applicative

-- | A purely formal Alternative structure over a functor, where
--   no particular constraints are made on the behaviour of 
--   'Or', 'Some' and 'Many'.
data Alt f a where
  Pure :: a -> Alt f a
  Lift :: f a -> Alt f a 
  Ap :: Alt f a -> Alt f (a -> b) -> Alt f b
  Empty :: Alt f a
  Or :: Alt f a -> Alt f a -> Alt f a
  Some :: Alt f a -> Alt f [a]

instance Functor f => Functor (Alt f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Lift g) = Lift $ fmap f g
  fmap f (Ap x g) = x `Ap` fmap (f .) g
  fmap _ Empty = Empty
  fmap f (Or a b) = Or (f <$> a) (f <$> b)
  fmap f (Some l) = f . (:[]) <$> l

instance Functor f => Applicative (Alt f) where
  pure = Pure
  {-# INLINE pure #-}
  y <*> f = Ap f y

instance Functor f => Alternative (Alt f) where
  empty = Empty
  a <|> b = Or a b
  some = Some 
  many x = some x <|> empty

runAlt :: forall f g a. Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt phi f = retractAlt $ hoistAlt phi f

liftAlt :: Functor f => f a -> Alt f a
liftAlt = Lift

hoistAlt :: forall f g a. (forall x. f x -> g x) -> Alt f a -> Alt g a
hoistAlt _ (Pure a) = Pure a
hoistAlt f (Lift g) = Lift $ f g
hoistAlt f (Ap x g) = Ap (hoistAlt f x) (hoistAlt f g)
hoistAlt _ Empty = Empty
hoistAlt f (Or a b) = Or (hoistAlt f a) (hoistAlt f b)
hoistAlt f (Some a) = Some $ hoistAlt f a

-- | Interpret the free alternative over f using the Alternative semantics for f.
retractAlt :: Alternative f => Alt f a -> f a
retractAlt (Pure a) = pure a
retractAlt (Lift f) = f
retractAlt (a `Ap` f) = retractAlt a <**> retractAlt f
retractAlt Empty = empty
retractAlt (Or a b) = retractAlt a <|> retractAlt b
retractAlt (Some a) = some $ retractAlt a
