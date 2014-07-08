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

data ArgumentDefault a = ArgumentDefault (Maybe a) (Maybe (a -> String))

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

data OptionGroup a where
  Single :: Option a -> OptionGroup a
  OneOf :: [Option a] -> OptionGroup a
  GroupOf :: OptionGroup b -> OptionGroup c -> (b -> c -> a) -> OptionGroup a

data Block a =
    SingleArgument (Argument a)
  | Subsection (OptionGroup a)

data Option a = Option {
    csNames :: [Name]
  , csDescription :: String
  , csBlock :: Block a
}