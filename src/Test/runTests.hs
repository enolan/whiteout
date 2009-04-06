import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.Internal.BEncode

tests :: [Test]
tests = [
    testGroup "Internal.BEncode" [
        testProperty "checkPack" checkPack,
        testProperty "checkReadPack" checkReadPack
        ]
    ]

main :: IO ()
main = defaultMain tests
