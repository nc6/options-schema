-- | Module to construct 'optparse-applicative'-style command-line parsers
--   from a generic schema.
--
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

{-# LANGUAGE GADTs #-}

module Options.Schema.Applicative (
  mkParser
) where

import Control.Applicative (empty)
import Control.Applicative.Free
import Data.Maybe (catMaybes)

import Options.Schema
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)

mkParser :: Schema a -> Parser a
mkParser = runAp mkOptionGroupParser

mkOptionGroupParser :: OptionGroup a -> Parser a
mkOptionGroupParser Empty = empty
mkOptionGroupParser (Single a) = mkOptionParser a
mkOptionGroupParser (OneOf as) = foldl1 (<|>) . map mkOptionParser $ as

mkOptionParser :: Option a -> Parser a
mkOptionParser (Option n d block) = mkBlockParser n d block

mkBlockParser :: [Name] -> Description -> Block a -> Parser a
mkBlockParser n d (SingleArgument a) = mkBasicParser n d a
mkBlockParser n d (Subsection a) = mkParser a --TODO currently this ignores names

-- | Make a basic parser for simple options
mkBasicParser :: [Name] -> Description -> Argument a -> Parser a
mkBasicParser n d (Argument
  argName argReader (ArgumentDefault def pp) desc) = let
    names = foldl1 (<>) . map mkName $ n
    props = foldl1 (<>) $ names :
      catMaybes [
          fmap help $ dSummary d
        , fmap value def
        , fmap (\x -> showDefaultWith (\_ -> x)) pp
        , fmap metavar argName
      ]
  in option argReader props

mkName :: HasName f => Name -> Mod f a
mkName name = case name of
  LongName n -> long n
  ShortName n -> short n