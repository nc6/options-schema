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

-- Infix notation for `Ap`
(<**>) :: Ap f (a->b) -> f a -> Ap f b
fun <**> arg = Ap arg fun

(<$$>) :: (a -> b) -> f a -> Ap f b
fun <$$> arg = Pure fun <**> arg

infixl 4 <**>, <$$>

------------ Basic Options ---------------------

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

strOption :: Mod String -> Option String
strOption = option (return . id)

-- TODO This should not be partial - should have a sensible reader
intOption :: Mod Int -> Option Int
intOption = option (return . read)

compositeOption :: Schema a -> Mod a -> Option a
compositeOption group (Mod f) = f $ Option {
    oNames = mempty
  , oDescription = mempty
  , oBlock = Subsection group
}

------ Lifting options into OptionGroups --------

one :: Option a -> OptionGroup a
one = Single

oneOf :: [Option a] -> OptionGroup a
oneOf = OneOf

------------- Option Modifiers ------------------

name :: Name -> Mod a
name n = Mod $ \a -> a { oNames = n : oNames a }

long :: String -> Mod a
long = name . LongName

short :: Char -> Mod a
short = name . ShortName

desc :: Description -> Mod a
desc str = Mod $ \a -> a { oDescription = oDescription a <> str }

summary :: String -> Mod a
summary d = desc (Description (Just d) Nothing)

detail :: String -> Mod a
detail d = desc (Description Nothing (Just d))

--- Must be a better way of doing these, surely?

type ArgMod a = (Argument a -> Argument a)

arg :: ArgMod a -> Mod a
arg mod = Mod $ \a -> case oBlock a of
  SingleArgument b -> a { oBlock = SingleArgument (mod b) }
  _ -> a

metavar :: String -> Mod a
metavar str = arg $ \b -> b { aMetavar = Just str }

reader :: Reader a -> Mod a
reader r = arg $ \b -> b { aReader = r }

argDesc :: Description -> Mod a
argDesc d = arg $ \b -> b { aDescr = aDescr b <> d }

value :: a -> Mod a
value d = arg $ \b -> let
    (ArgumentDefault _ y) = aDefault b
    def' = ArgumentDefault (Just d) y
  in b { aDefault = def' }

valueShow :: (a -> String) -> Mod a
valueShow d = arg $ \b -> let
    (ArgumentDefault x _) = aDefault b
    def' = ArgumentDefault x (fmap d x)
  in b { aDefault = def' }

argSummary :: String -> Mod a
argSummary d = argDesc (Description (Just d) Nothing)

argDetail :: String -> Mod a
argDetail d = argDesc (Description Nothing (Just d))