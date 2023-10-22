module Main where

import qualified CreditCardEx (toDigit)
import qualified Control.Applicative as MyLib
import Data.Type.Equality (TestEquality(testEquality))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let x = CreditCardEx.toDigit 95240687
  print x

