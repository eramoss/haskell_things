import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified Bendecoder as B
import Test.HUnit.Lang (HUnitFailure(HUnitFailure))

decodeStringTest :: Assertion
decodeStringTest = assertEqual "default string test" "some str" (B.decodeString "8:some str")

decodeWrongStringTest :: Assertion
decodeWrongStringTest =  assertEqual "decode wrong" "some st" (B.decodeString "7:some str")

main :: IO ()
main = defaultMainWithOpts
       [testCase "default string test" decodeStringTest
       ,testCase "fail in decode with wrong len" decodeWrongStringTest]
       mempty
