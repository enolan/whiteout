import Test.Bencode
import Test.QuickCheck

main = quickCheckWith stdArgs checkGetPut
