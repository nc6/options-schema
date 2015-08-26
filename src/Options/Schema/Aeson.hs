{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- | Module to construct aeson parsers
--   from a generic schema.
--
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

module Options.Schema.Aeson where

import Control.Applicative
  ( Alternative
  , empty
  , (<|>)
  )
import Control.Alternative.FreeStar

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Options.Schema

newtype Parser a = Parser { runParser :: A.Value -> A.Parser a }
  deriving (Functor, Monoid)

instance Applicative Parser where
  pure a = Parser $ \_ -> pure a
  (Parser a) <*> (Parser b) = Parser $ \val -> (a val <*> b val)

instance Alternative Parser where
  empty = Parser $ \_ -> empty
  (Parser a) <|> (Parser b) = Parser $ \val -> (a val <|> b val)

mkParser :: Schema a -> Parser a
mkParser = runAlt mkOptionParser

mkOptionParser :: Option a -> Parser a
mkOptionParser (Option n _ block) = mkBlockParser n block

mkBlockParser :: [Name] -> Block a -> Parser a
mkBlockParser n (SingleArgument a) = mkBasicParser n a
mkBlockParser sn (Subsection a) = defaultOption def $
    Parser . A.withObject ("Failed to parse config section for option: " ++ show name)
      $ \entries -> case M.lookup (T.pack name) entries of
        Just (A.Object e) -> \e -> mkParser

mkBasicParser :: [Name] -> Argument a -> Parser a
mkBasicParser n (Argument _ reader def _) = defaultOption def $
    Parser . A.withObject ("Failed to parse argument for option: " ++ show name)
      $ \entries -> case M.lookup (T.pack name) entries of
        Just (A.String e) -> reader . T.unpack $ e
        _ -> empty
  where
    name = principalName n

defaultOption :: ArgumentDefault a -> Parser a -> Parser (a)
defaultOption (ArgumentDefault def _) = case def of
  Just x -> \a -> a <|> pure x
  Nothing -> id

principalName :: [Name] -> String
principalName n = case filterMap eitherName n of
  (x:_, _) -> x
  ([], x:_) -> "option_" ++ [x]
  _ -> error $ "No names provided for option!"

-- | Extract either name from their constructors
eitherName :: Name -> Either String Char
eitherName (LongName x) = Left x
eitherName (ShortName x) = Right x

filterMap :: (a -> Either b c) -> [a] -> ([b], [c])
filterMap f as = go as ([], []) where
  go [] r = r
  go (x:xs) (b,c) = go xs $ case f x of
    Left y -> (y:b, c)
    Right y -> (b, y:c)

