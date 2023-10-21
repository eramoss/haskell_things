module Main where

import qualified MyLib (someFunc)
import qualified Control.Applicative as MyLib

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
