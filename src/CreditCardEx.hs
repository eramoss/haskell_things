module CreditCardEx (toDigit) where
import Data.Char (ord, digitToInt)
import Data.Fixed (div', mod')

toDigit:: Integer -> [Integer]
toDigit 0 = []
toDigit n = toDigit (n `div` 10) ++ [n `mod` 10]