-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

module Options.Schema.Applicative (
  mkParser
) where

import Data.Maybe (catMaybes)

import Options.Schema
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)

mkParser :: OptionGroup a -> Parser a
mkParser (Single a) = mkOptionParser a
mkParser (OneOf as) = foldl1 (<|>) . map mkOptionParser $ as
mkParser (GroupOf b c f) = let
    p1 = mkParser b
    p2 = mkParser c
  in f <$> p1 <*> p2

mkOptionParser :: Option a -> Parser a
mkOptionParser (Option n d block) = mkBlockParser n d block

mkBlockParser :: [Name] -> String -> Block a -> Parser a
mkBlockParser n d (SingleArgument a) = mkBasicParser n d a
mkBlockParser n d (Subsection a) = mkParser a

-- | Make a basic parser for simple options
mkBasicParser :: [Name] -> String -> Argument a -> Parser a
mkBasicParser n d (Argument
  argReader (ArgumentDefault def pp) desc) = let
    names = foldl1 (<>) . map mkName $ n
    props = foldl1 (<>) $ names : [
          help d
        , reader argReader
      ] ++ catMaybes [
          fmap value def
        , fmap showDefaultWith pp
        , fmap metavar $ adMetavar desc
      ]
  in nullOption props

mkName :: HasName f => Name -> Mod f a
mkName name = case name of
  LongName n -> long n
  ShortName n -> short n