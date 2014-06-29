import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Data.Aeson (encode, decode')

import APIJson
import MozillaApi (Node, Program)

main = interact $ process


process :: String -> String
process = unpack . encode . (fromJust :: Maybe (Node Program) -> Node Program) . decode'. pack
