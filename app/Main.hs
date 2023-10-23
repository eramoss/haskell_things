module Main where

import qualified CreditCardEx (isValidCardNumber)
import qualified Control.Applicative as MyLib
import Data.Type.Equality (TestEquality(testEquality))
import CreditCardEx (toDigit, splitInDigits)
import Data.Char (isAlpha, isAlphaNum, isNumber, digitToInt)


decodeNumber :: String -> Integer
decodeNumber ('e': rest) = decodeNumber (reverse rest)
decodeNumber ('i': rest) = decodeNumber (reverse rest)
decodeNumber rest
    | isNumber (firstLetter rest) = read rest
    | otherwise = error ("Invalid number on decodeNumber: " ++ rest)

firstLetter :: String -> Char
firstLetter (s:ss) = s 

decodeString :: String -> String
decodeString s = readStringPair( transformPairOfString(getPair s) )

readStringPair:: (Integer,String) -> String
readStringPair (0, _) = ""
readStringPair (x, []) = ""
readStringPair (x, s: ss) =  s: readStringPair (x-1, ss)

getPair:: String -> (String,String)
getPair (':':ss) = ("",ss)
getPair (s:ss) 
    | isNumber s = ( s : firstElement (getPair ss), secondElement (getPair ss))

transformPairOfString :: (String, String) -> (Integer, String)
transformPairOfString (x,y) = (read x, y)

firstElement :: (a, b) -> a
firstElement (x, _) = x
secondElement :: (a, b) -> b
secondElement (_, x) = x

main :: IO ()
main = do
    let encoded = "14:iam the string"
    print (decodeString encoded)

