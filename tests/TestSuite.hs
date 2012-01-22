import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.ByteString as BS
import Network.SPDY as SP
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests = [testGroup "FrameParsing" [
	      testCase "Noop" testNoop
	    , testCase "Ping" testPing
	    , testCase "SynStream" testSynStream]]

testParseFrame :: BS.ByteString -> SP.Frame -> Assertion
testParseFrame bs frame = Just frame @?= SP.parse bs

testNoop = testParseFrame bs fr
    where
	bs = (BS.pack [128, 2, 0, 5, 0, 0, 0, 0]) 
	fr = Noop (ControlFrameHeader 2 Set.empty 0)


testPing = testParseFrame bs fr
    where
	fr = Ping (ControlFrameHeader 2 Set.empty 4) 123
	bs = BS.pack [128, 2, 0, 6
		     , 0, 0, 0, 4 
        	     , 0, 0, 0, 123]

testSynStream = testParseFrame bs fr
    where
	fr = SynStream (ControlFrameHeader 2 Set.empty 12) 0 1 Nothing headers
	headers = Map.fromList [("host", "localhost")
			       ,("custom", "1")] 
	bs = BS.pack [ 128, 2, 0, 1,
		       0, 0, 0, 12, 
		       0x00, 0x00, 0x00, 0x01, -- Stream ID
		       0x00, 0x00, 0x00, 0x00, -- Associated Stream ID
		       0x00, 0x00, -- Priority + Unused
		       0x78, 0xbb, 0xdf, 0xa2, 0x51, 0xb2, --Deflated Name/Value pairs
                       0x62, 0x60, 0x62, 0x60, 0x01, 0xe5, 0x12,
		       0x06, 0x4e, 0x50, 0x50, 0xe6, 0x80, 0x99,
		       0x6c, 0xc9, 0xa5, 0xc5, 0x25, 0xf9, 0xb9,
                       0x0c, 0x8c, 0x86, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]

