module Main where

import qualified CreditCardEx (isValidCardNumber)
import qualified Bendecoder (decodeBencodedValue)



main :: IO ()
main = do
    let encoded = "35:haskjdgjasgdjgfqwuyegafsdagsdfjy"
    print (Bendecoder.decodeBencodedValue encoded)

