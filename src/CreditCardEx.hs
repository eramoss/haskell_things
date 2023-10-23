module CreditCardEx (isValidCardNumber,splitInDigits,toDigit,reverseToDigit,sumDigits,doubleEveryTwo) where

toDigit:: Integer -> [Integer]
toDigit 0 = []
toDigit n = toDigit (n `div` 10) ++ [n `mod` 10]


reverseToDigit :: Integer -> [Integer]
reverseToDigit 0 = []
reverseToDigit n = reverse( toDigit n)


isValidCardNumber :: Integer -> Bool
isValidCardNumber n  = calcValidCardNumber( toDigit n)

calcValidCardNumber :: [Integer] -> Bool
calcValidCardNumber [] = False
calcValidCardNumber xs
   = (sumDigits (splitInDigits (doubleEveryTwo (reverse xs))) `mod` 10) == 0


splitInDigits:: [Integer] -> [Integer]
splitInDigits [] = [] -- case empty list do nothing
splitInDigits (x:xs) = (toDigit x) ++ splitInDigits xs

sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits(xs)

doubleEveryTwo :: [Integer] -> [Integer]
doubleEveryTwo [] = []
doubleEveryTwo [x] = [x]
doubleEveryTwo (x:y:zs) = x:( y*2) : doubleEveryTwo zs