-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

import Control.Applicative
import Data.Monoid

import qualified Options.Applicative as CL
import Options.Schema
import Options.Schema.Applicative
import Options.Schema.Builder

data SubOpts = SubOpts Int deriving (Eq, Show)

data MyOpts = MyOpts String String SubOpts deriving (Eq, Show)

defaultShow :: Show a => a -> ArgumentDefault a
defaultShow x = ArgumentDefault (Just x) (Just $ show x)

foo :: Option String
foo = strOption $ long "foo" <> short 'f'
                <> summary "The foo argument"
                <> detail "Some more detail about the foo argument"
                <> metavar "FOO"
                <> value "foo_arg"

bar :: Option String
bar = strOption $ long "bar" <> short 'f'
                <> summary "The bar argument"
                <> detail "Some more detail about the bar argument"
                <> metavar "BAR"
                <> value "bar_arg"

qux :: Option Int
qux = intOption $ long "qux" <> short 'f'
                <> summary "The qux argument"
                <> detail "Some more detail about the qux argument"
                <> metavar "QUX"
                <> value 42

mySubOpts :: Option SubOpts
mySubOpts = subOption subOpts
            $  long "baz"
            <> summary "The baz subsection"
  where
    subOpts = Subsection . liftAp . Single $ fmap SubOpts qux

myOpts :: Schema MyOpts
myOpts = Ap (Single mySubOpts) (Ap (Single bar) (Ap (Single foo) (Pure MyOpts)))

optParser :: CL.Parser MyOpts
optParser = mkParser myOpts

main :: IO ()
main = CL.execParser opts >>= (putStrLn . greet) where
  opts = CL.info (CL.helper <*> optParser)
          ( CL.fullDesc
            <> CL.progDesc "Print a greeting for TARGET"
            <> CL.header "hello - a test for options-schema" )

greet :: MyOpts -> String
greet = show
