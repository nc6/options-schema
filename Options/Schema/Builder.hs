-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

{-# LANGUAGE RankNTypes #-}
module Options.Schema.Builder where

import Control.Applicative.Free
import Data.Monoid
import Options.Schema

-- This should really be MonadFail...
type Reader a = forall m. Monad m => String -> m a

-- Option modifier.
data Mod a = Mod (Option a -> Option a)

instance Monoid (Mod a) where
  mempty = Mod id
  (Mod f) `mappend` (Mod g) = Mod $ f . g

-- | Infix notation for `Ap`
(<**>) :: Ap f (a->b) -> f a -> Ap f b
fun <**> arg = Ap arg fun

-- | Equivalent of <$>
(<$$>) :: (a -> b) -> f a -> Ap f b
fun <$$> arg = Pure fun <**> arg

infixl 4 <**>, <$$>

------------ Basic Options ---------------------

-- | Construct a basic option given a reader for the type.
option :: Reader a -> Mod a -> Option a
option rdr (Mod f) = f $ Option {
    oNames = mempty
  , oDescription = mempty
  , oBlock = SingleArgument $ Argument {
        aMetavar = mempty
      , aReader = rdr
      , aDefault = ArgumentDefault Nothing Nothing
      , aDescr = mempty
  }
}

-- | Construct an option using the default 'String' reader.
strOption :: Mod String -> Option String
strOption = option (return . id)

-- TODO This should not be partial - should have a sensible reader
-- | Construct an option using the default 'Int' reader.
intOption :: Mod Int -> Option Int
intOption = option (return . read)

-- | Construct an option from a sub-schema.
compositeOption :: Schema a -> Mod a -> Option a
compositeOption group (Mod f) = f $ Option {
    oNames = mempty
  , oDescription = mempty
  , oBlock = Subsection group
}

-- | Make a mandatory argument optional with a default of `Nothing`
optional :: Option a -> Option (Maybe a)
optional = def . fmap return where
  (Mod def) = valueShow (\_ -> "Nothing") <> value Nothing

-- | Make an argument optional with a delineated default, using an appropriate
--   show instance to display the default.
defaultableShow :: Show a => a -> Option a -> Option (Defaultable a)
defaultableShow a = def . fmap Configured where
  (Mod def) = valueShow (\_ -> show a) <> value (Default a)

-- | Make an argument optional with a delineated default.
defaultable :: a -- ^ Default value.
            -> (a -> String) -- ^ Function to display default value.
            -> Option a -- ^ Existing option.
            -> Option (Defaultable a)
defaultable a pa = def . fmap Configured where
  (Mod def) = valueShow (\_ -> pa a) <> value (Default a)

------ Lifting options into OptionGroups --------

-- | Construct an @OptionGroup@ containing a single option.
one :: Option a -> OptionGroup a
one = Single

-- | Construct an @OptionGroup@ consisting of a number of
--   alternate options.
oneOf :: [Option a] -> OptionGroup a
oneOf = OneOf

------------- Option Modifiers ------------------

name :: Name -> Mod a
name n = Mod $ \a -> a { oNames = n : oNames a }

-- | Specify a long name for the option.
long :: String -> Mod a
long = name . LongName

-- | Specify a short name for the option. Short names are mostly used in
--   command-line parsers.
short :: Char -> Mod a
short = name . ShortName

-- | Specify the option description. This description applies to the whole
--   option, not simply the argument value.
desc :: Description -> Mod a
desc str = Mod $ \a -> a { oDescription = oDescription a <> str }

-- | Specify the option summary. This should be a short precis of the option.
summary :: String -> Mod a
summary d = desc (Description (Just d) Nothing)

-- | Specify the option detail. This should be a full explanation of the option,
--   its use, and its effect on the application.
detail :: String -> Mod a
detail d = desc (Description Nothing (Just d))

--- Must be a better way of doing these, surely?

type ArgMod a = (Argument a -> Argument a)

arg :: ArgMod a -> Mod a
arg mod = Mod $ \a -> case oBlock a of
  SingleArgument b -> a { oBlock = SingleArgument (mod b) }
  _ -> a

-- | Specify the default name for the argument - e.g. for a command-line parser,
--   this might appear as '--foo FOO', where 'FOO' is the metavar.
metavar :: String -> Mod a
metavar str = arg $ \b -> b { aMetavar = Just str }

-- | Specify the option reader. The reader is used to construct a value of
--   correct type from a string.
reader :: Reader a -> Mod a
reader r = arg $ \b -> b { aReader = r }

argDesc :: Description -> Mod a
argDesc d = arg $ \b -> b { aDescr = aDescr b <> d }

-- | Specify the default value for the argument.
value :: a -> Mod a
value d = arg $ \b -> let
    (ArgumentDefault _ y) = aDefault b
    def' = ArgumentDefault (Just d) y
  in b { aDefault = def' }

-- | Specify a function used to display the default argument. Note that,
--   confusingly, this must be specified *before* the default value.
valueShow :: (a -> String) -> Mod a
valueShow d = arg $ \b -> let
    (ArgumentDefault x _) = aDefault b
    def' = ArgumentDefault x (fmap d x)
  in b { aDefault = def' }

-- | Specify the argument summary. This description applies only to the
--   argument. Eg. one may have "Set the message timeout" as the option summary,
--   with "Number of seconds" as the argument summary.
argSummary :: String -> Mod a
argSummary d = argDesc (Description (Just d) Nothing)

-- | Specify the argument detail. This description applies only to the
--   argument.
argDetail :: String -> Mod a
argDetail d = argDesc (Description Nothing (Just d))