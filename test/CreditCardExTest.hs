import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified CreditCardEx (toDigit, reverseToDigit, splitInDigits, sumDigits,doubleEveryTwo)

toDigitTest :: Assertion
toDigitTest = assertEqual "toDigit test with zero" [5,3,4] (CreditCardEx.toDigit 0534)

reverseToDigitTest :: Assertion
reverseToDigitTest = assertEqual "reverseToDigit default test" [5,3,4] (CreditCardEx.reverseToDigit 435)

splitInDigitsTest :: Assertion
splitInDigitsTest = assertEqual "splitInDigits" [5,1,3,5] (CreditCardEx.splitInDigits [5,13,5])

sumDigitsTest :: Assertion
sumDigitsTest = assertEqual "sumDigits" 15 (CreditCardEx.sumDigits [5,5,5])

doubleEveryOtherTest :: Assertion
doubleEveryOtherTest = assertEqual "doubleEveryOther" [3,4,0,20,5,0]
       (CreditCardEx.doubleEveryTwo[3,2,0,10,5,0])

main :: IO ()
main = defaultMainWithOpts
       [testCase "to digit" toDigitTest
       ,testCase "reverse to digit" reverseToDigitTest
       ,testCase "split digits" splitInDigitsTest
       ,testCase "sum digits" sumDigitsTest
       ,testCase "double every other" doubleEveryOtherTest]
       mempty
