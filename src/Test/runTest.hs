import Test.Internal.BEncode
import Test.QuickCheck

main :: IO ()
main = quickCheckWith stdArgs checkReadPack
