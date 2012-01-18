import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.ByteString as BS
import Network.SPDY as SP
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests = [testGroup "G1" [
	      testCase "test" testParse]]

testPing = BS.pack [128, 2, 0, 6, 0, 0, 0, 6, 0, 0, 0, 1]
testParse = SP.parse testPing @?= Just pingFrame
    where
	pingFrame = Ping (ControlFrameHeader 1 Set.empty 0) 1
