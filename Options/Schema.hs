-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Options.Schema where

data Name =
    LongName !String
  | ShortName !Char
  deriving (Eq, Ord)

data ArgumentDefault a = ArgumentDefault (Maybe a) (Maybe String)

instance Functor ArgumentDefault where
  fmap f (ArgumentDefault val pp) = ArgumentDefault (fmap f val) pp

data ArgumentDescr = ArgumentDescr {
    adSummary :: String
  , adDetail :: String
  , adMetavar :: Maybe String
}

data Argument a = Argument {
    aReader :: forall m. Monad m => String -> m a
  , aDefault :: ArgumentDefault a
  , aDescr :: ArgumentDescr
}

instance Functor Argument where
  fmap f (Argument r def descr) = (Argument (\s -> r s >>= return . f) def' descr) where
    def' = fmap f def

data OptionGroup a where
  Single :: Option a -> OptionGroup a
  OneOf :: [Option a] -> OptionGroup a
  ConsOf :: (b -> c -> a) -> Option b -> OptionGroup c -> OptionGroup a

instance Functor OptionGroup where
  fmap f (Single opt) = Single (fmap f opt)
  fmap f (OneOf opts) = OneOf . (fmap . fmap) f $ opts
  fmap f (ConsOf t first rest) = ConsOf (curry $ f . uncurry t) first rest

data Block a =
    SingleArgument (Argument a)
  | Subsection (OptionGroup a)

instance Functor Block where
  fmap f (SingleArgument x) = SingleArgument $ fmap f x
  fmap f (Subsection x) = Subsection $ fmap f x

data Option a = Option {
    csNames :: [Name]
  , csDescription :: String
  , csBlock :: Block a
}

instance Functor Option where
  fmap f (Option n d b) = Option n d (fmap f b)