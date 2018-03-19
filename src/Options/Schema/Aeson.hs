{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module to construct aeson parsers
--   from a generic schema.
--
-- Copyright : (C) 2018 Nicholas Clarke
-- License   : All rights reserved.

module Options.Schema.Aeson
  ( mkParser
  , parseMaybe
  ) where

import           Control.Alternative.Freer
import           Control.Applicative       (Alternative, empty, (<|>))
import qualified Data.Aeson                as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson.Types          as A
import qualified Data.HashMap.Strict       as M
import qualified Data.Text                 as T
import           Options.Schema
import qualified Data.Vector as V

newtype Parser a = Parser { runParser :: A.Value -> A.Parser a }
  deriving (Functor, Monoid)

instance Applicative Parser where
  pure a = Parser $ const $ pure a
  (Parser a) <*> (Parser b) = Parser $ \val -> a val <*> b val

instance Alternative Parser where
  empty = Parser $ const empty
  Parser a <|> Parser b = Parser $ \val -> a val <|> b val

-- | Make a parser for a schema
mkParser :: Schema a -> Parser a
mkParser (Pure a) = Parser $ const . pure $ a
mkParser (Lift (Option names _ block)) = mkBlockParser names block
mkParser (Ap a f) = mkParser f <*> mkParser a
mkParser Empty = empty
mkParser (Or a b) = mkParser a <|> mkParser b
mkParser (Some a) = Parser . A.withArray "Cannot parse array." $
  \arr -> mapM (runParser $ mkParser a) $ V.toList arr

-- | Make a flag parser
mkFlagParser :: a -> a -> Parser a
mkFlagParser active def = Parser . A.withBool "Cannot parse flag." $
  \b -> return $ if b then active else def

-- | Make a parser for a single string argument
mkSingleArgumentParser :: Argument a -> Parser a
mkSingleArgumentParser (Argument _ reader def _) =
  Parser . A.withText "Cannot parse value" $
    \txt -> reader $ T.unpack txt

mkBlockParser :: [Name] -> Block a -> Parser a
mkBlockParser names block =
    mkBlockParser' $ case block of
     SingleArgument a -> mkSingleArgumentParser a
     Flag active def  -> mkFlagParser active def
     Subsection sch   -> mkParser sch
  where
    mkBlockParser' subParser =
      Parser . A.withObject "Cannot parse block." $
        \b -> case findByNames names b of
          Just val -> runParser subParser val
          Nothing  -> error $ "Field " ++ show names ++ " not present."
    findByNames :: [Name] -> M.HashMap T.Text A.Value -> Maybe A.Value
    findByNames nms obj = case nms of
      [] -> Nothing
      (x:xs) -> case M.lookup (nameText x) obj of
        Nothing -> findByNames xs obj
        Just v  -> Just v
    nameText (LongName n)  = T.pack n
    nameText (ShortName s) = T.singleton s

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe (Parser f) bs = A.parseMaybe f =<< A.decode bs
