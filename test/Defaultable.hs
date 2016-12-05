-- |
-- Copyright : (C) 2016 Seagate Technology Limited.
-- License   : BSD3

import Control.Applicative ((<*>), (<$>), pure)
import Data.Defaultable
import Data.Monoid

import qualified Options.Applicative as CL
import Options.Schema
import Options.Schema.Applicative
import Options.Schema.Builder

data MyOpts = MyOpts (Defaultable Int) (Maybe Int) deriving (Eq, Show)

myOpts :: Schema MyOpts
myOpts = MyOpts <$> qaz <*> qux

qaz :: Schema (Defaultable Int)
qaz = defaultable 4 . intOption $ long "qaz"
                <> summary "The qaz argument"
                <> detail "Some more detail about the qaz argument"
                <> metavar "QAZ"
                -- <> valueShow show
                -- <> value 23

qux :: Schema (Maybe Int)
qux = optional . intOption $ long "qux" <> short 'q'
                <> summary "The qux argument"
                <> detail "Some more detail about the qux argument"
                <> metavar "QUX"

main :: IO ()
main = CL.execParser opts >>= (putStrLn . show) where
  opts = CL.info (CL.helper <*> mkParser myOpts)
          ( CL.fullDesc
            <> CL.progDesc "Print a greeting for TARGET"
            <> CL.header "hello - a test for options-schema" )
