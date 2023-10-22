import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified CreditCardEx (toDigit, reverseToDigit)

toDigitTest :: Assertion
toDigitTest = assertEqual "toDigit test with zero" [5,3,4] (CreditCardEx.toDigit 0534)

reverseToDigitTest :: Assertion
reverseToDigitTest = assertEqual "reverseToDigit default test" [5,3,4] (CreditCardEx.reverseToDigit 435)

main :: IO ()
main = defaultMainWithOpts
       [testCase "to digit" toDigitTest
       ,testCase "reverse to digit" reverseToDigitTest]
       mempty
