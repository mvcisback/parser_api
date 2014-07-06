{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck.All
import Test.QuickCheck

import APIJson
import MozillaApi (Node, Program)
import Generate
import Data.DeriveTH

import Data.Aeson (encode, decode')

prop_encodeDecodeInvariant :: Node Program -> Bool
prop_encodeDecodeInvariant x = Just x == ( decode' . encode) x

return [] 
main = $quickCheckAll
