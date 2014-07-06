import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Lazy.Char8 as B

import Data.Aeson (encode, decode')

import APIJson
import MozillaApi (Node, Program)

main = B.getContents >>= (print . process)


process = encode . (fromJust :: Maybe (Node Program) -> Node Program) . decode'
