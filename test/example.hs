-- |
-- Copyright : (C) 2013 Xyratex Technology Limited.
-- License   : All rights reserved.

import Control.Applicative ((<*>), (<$>), pure)
import Data.Defaultable
import Data.Monoid

import qualified Options.Applicative as CL
import Options.Schema
import Options.Schema.Applicative
import Options.Schema.Builder

data SubOpts = SubOpts (Defaultable Int) (Maybe String) deriving (Eq, Show)

data MyOpts = MyOpts (Maybe Int) String SubOpts deriving (Eq, Show)

defaultShow :: Show a => a -> ArgumentDefault a
defaultShow x = ArgumentDefault (Just x) (Just $ show x)

foo :: Schema String
foo = strOption $ long "foo" <> short 'f'
                <> summary "The foo argument (mandatory)."
                <> detail ("The foo argument is necessary. It should be a " ++
                           "parser error to fail to include it.")
                <> metavar "FOO"

bar :: Schema (Maybe String)
bar = optional . strOption $ long "bar" <> short 'b'
                <> summary "The bar argument"
                <> detail "Some more detail about the bar argument"
                <> metavar "BAR"

qux :: Schema (Maybe Int)
qux = optional . intOption $ long "qux" <> short 'q'
                <> summary "The qux argument"
                <> detail "Some more detail about the qux argument"
                <> metavar "QUX"

qaz :: Schema (Defaultable Int)
qaz = defaultable 4 . intOption $ long "qaz"
                <> summary "The qaz argument"
                <> detail "Some more detail about the qaz argument"
                <> metavar "QAZ"
                -- <> valueShow show
                -- <> value 23

mySubOpts :: Schema SubOpts
mySubOpts = compositeOption subOpts
            $  long "baz"
            <> summary "The baz subsection"
  where
    subOpts = SubOpts <$> qaz <*> bar

myOpts :: Schema MyOpts
myOpts = MyOpts <$> qux <*> foo <*> mySubOpts

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
