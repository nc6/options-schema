-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

import Options.Applicative
import Options.Schema
import Options.Schema.Applicative

data SubOpts = SubOpts Int deriving (Eq, Show)

data MyOpts = MyOpts String String SubOpts deriving (Eq, Show)

defaultShow :: Show a => a -> ArgumentDefault a
defaultShow x = ArgumentDefault (Just x) (Just $ show x)

foo :: Option String
foo = Option {
    csNames = [LongName "foo", ShortName 'f']
  , csDescription = "The foo argument."
  , csBlock = SingleArgument (
      Argument return (defaultShow "foo_arg") (ArgumentDescr "Foo" "More foo" (Just "FOO")))
}

bar :: Option String
bar = Option {
    csNames = [LongName "bar", ShortName 'f']
  , csDescription = "The bar argument."
  , csBlock = SingleArgument (
      Argument return (defaultShow "bar_arg") (ArgumentDescr "Bar" "More bar" (Just "BAR")))
}

qux :: Option Int
qux = Option {
    csNames = [LongName "qux", ShortName 'f']
  , csDescription = "The qux argument."
  , csBlock = SingleArgument (
      Argument (return . read) (defaultShow 5) (ArgumentDescr "Bar" "More qux" Nothing))
}

mySubOpts :: Option SubOpts
mySubOpts = mkOpt . Subsection $ fmap SubOpts $ Single qux where
  mkOpt x = Option {
      csNames = [LongName "baz"]
    , csDescription = "The baz subsection"
    , csBlock = x
  }

myOpts :: OptionGroup MyOpts
myOpts = fmap (\(a,(b,c)) -> MyOpts a b c) $
  ConsOf (,) foo $ ConsOf (,) bar (Single mySubOpts)

optParser :: Parser MyOpts
optParser = mkParser myOpts

main :: IO ()
main = execParser opts >>= (putStrLn . greet) where
  opts = info (helper <*> optParser)
          ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for options-schema" )

greet :: MyOpts -> String
greet = show
