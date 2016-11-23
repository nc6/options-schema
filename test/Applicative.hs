-- |
-- Copyright : (C) 2014 Seagate Technology Limited.
-- License   : BSD3

import Control.Applicative
import Control.Alternative.Freer

import qualified Options.Applicative as CL
import qualified Options.Applicative.Help.Core as CL

data Foo x = Foo {
    name :: String
  , reader :: String -> x
}

instance Functor Foo where
  fmap f (Foo n r) = Foo n $ f . r

mkParser :: Foo a -> CL.Parser a
mkParser (Foo n _) = CL.option CL.disabled ( CL.long n )

type Bar a = Alt Foo a

foo :: String -> (String -> x) -> Bar x
foo n r = liftAlt $ Foo n r

myFoo :: Bar [String]
myFoo = many $ foo "Hello" (\_ -> "Hello")

clFoo :: CL.Parser [String]
clFoo = runAlt mkParser $ myFoo

hangs = CL.cmdDesc clFoo

main :: IO ()
main = print hangs
