import Test.Framework

import Test.Internal.BEncode
import Test.Network.Whiteout

tests :: [Test]
tests = [
    Test.Internal.BEncode.theTests,
    Test.Network.Whiteout.theTests
    ]

main :: IO ()
main = defaultMain tests
