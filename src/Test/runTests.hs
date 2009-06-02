import Test.Framework

import Test.Internal.BEncode
import Test.Internal.Peer
import Test.Network.Whiteout

tests :: [Test]
tests = [
    Test.Internal.BEncode.theTests,
    Test.Internal.Peer.theTests,
    Test.Network.Whiteout.theTests
    ]

main :: IO ()
main = defaultMain tests
