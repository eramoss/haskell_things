module Main where

import qualified CreditCardEx (isValidCardNumber)
import qualified Control.Applicative as MyLib
import Data.Type.Equality (TestEquality(testEquality))

main :: IO ()
main = do
    let cardNumber = 4012888888881881
    let isValid = CreditCardEx.isValidCardNumber cardNumber
    putStrLn $ if isValid then "Valid" else "Invalid"

