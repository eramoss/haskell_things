module CreditCardEx (toDigit, reverseToDigit) where
import Data.Char (ord, digitToInt)
import Data.Fixed (div', mod')

toDigit:: Integer -> [Integer]
toDigit 0 = []
toDigit n = toDigit (n `div` 10) ++ [n `mod` 10]

reverseToDigit :: Integer -> [Integer]
reverseToDigit 0 = []
reverseToDigit n = reverse( toDigit n)