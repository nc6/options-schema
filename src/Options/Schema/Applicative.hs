-- | Module to construct 'optparse-applicative'-style command-line parsers
--   from a generic schema.
--
-- Copyright : (C) 2014 Seagate Technology Limited.
-- License   : BSD3

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Options.Schema.Applicative (
  mkParser
) where

import Control.Alternative.FreeStar
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Options.Schema
import Options.Applicative
import Options.Applicative.Types (readerAsk, ReadM(..))
import Options.Applicative.Builder.Internal (HasName)

mkParser :: Schema a -> Parser a
mkParser = runAlt mkOptionParser

mkOptionParser :: Option a -> Parser a
mkOptionParser (Option n d block) = mkBlockParser n d block

mkBlockParser :: [Name] -> Description -> Block a -> Parser a
mkBlockParser n d (SingleArgument a) = mkBasicParser n d a
mkBlockParser sn _ (Subsection a) = mkParser . (hoistAlt go) $ a
  where
    go :: Option a -> Option a
    go (Option n d b) = Option (fmap go' n) d b
    go' = \case
      LongName x -> LongName $ fstName ++ "-" ++ x
      ShortName x -> LongName $ fstName ++ "-" ++ [x]
    fstName = case (head sn) of
      LongName x -> x
      ShortName x -> [x]


-- | Make a basic parser for simple options
mkBasicParser :: [Name] -> Description -> Argument a -> Parser a
mkBasicParser n d (Argument
  argName argReader (ArgumentDefault def pp) _) = let
    names = foldl1 (<>) . map mkName $ n
    props = foldl1 (<>) $ names :
      catMaybes [
          fmap help $ dSummary d
        , fmap value def
        , fmap (\x -> showDefaultWith (\_ -> x)) pp
        , fmap metavar argName
      ]
  in option (mkReadM argReader) props

mkName :: HasName f => Name -> Mod f a
mkName name = case name of
  LongName n -> long n
  ShortName n -> short n

mkReadM :: (String -> Except ParseError a) -> ReadM a
mkReadM oldReader = readerAsk >>= ReadM . lift . oldReader
