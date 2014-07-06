import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Lazy.Char8 as B

import Data.Aeson (encode, decode')

import APIJson
import MozillaApi (Node, Program)
import Generate
import Data.DeriveTH

import Test.QuickCheck

main = quickCheck encodeDecodeInvariant

encodeDecodeInvariant :: Node Program -> Bool
encodeDecodeInvariant x = Just x == ( decode' . encode) x
 
