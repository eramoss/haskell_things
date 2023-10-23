module Main where

import qualified CreditCardEx (isValidCardNumber)
import qualified Bendecoder (decodeBencodedValue,replace)



main :: IO ()
main = do
    let encoded = "l3:foo3:bare4:spam4:eggsei42e3:quxe"
    print (init (Bendecoder.replace ",]" "]," (Bendecoder.decodeBencodedValue encoded)) )


