import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Test.Internal.BEncode
import Test.Network.Whiteout

tests :: [Test]
tests = [
    testGroup "Internal.BEncode" [
        testProperty "checkPack" checkPack,
        testProperty "checkReadPack" checkReadPack
        ],
    testGroup "Network.Whiteout" [
        testCase "verifySingleFileShouldSucceed" verifySingleFileShouldSucceed,
        testCase "verifySingleFileShouldFail" verifySingleFileShouldFail
        ]
    ]

main :: IO ()
main = defaultMain tests
