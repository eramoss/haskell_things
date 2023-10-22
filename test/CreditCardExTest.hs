import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified CreditCardEx (toDigit)

toDigitTest :: Assertion
toDigitTest = assertEqual "toDigit test with zero" [5,3,4] (CreditCardEx.toDigit 0534)



main :: IO ()
main = defaultMainWithOpts
       [testCase "to digit" toDigitTest]
       mempty