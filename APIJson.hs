{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON,ToJSON,(.:),object,(.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>),(<*>),empty)
import Data.Text (Text)

import MozillaApi

instance FromJSON SourceLocation
instance ToJSON SourceLocation

instance FromJSON Position
instance ToJSON Position

instance FromJSON Identifier where
    parseJSON (A.Object v) = Identifier <$> v .: "name"
    parseJSON _ = empty

instance ToJSON Identifier where
    toJSON (Identifier name) = object [ "name" .= name ]

instance FromJSON Literal where
    parseJSON (A.Object v) = makeLit <$> v .: "value"
        where makeLit :: Text -> Literal
              makeLit val
                  | val == "string" = StringLit
                  | val == "boolean" = BoolLit
                  | val == "number" = Number
                  | val == "RegExp" = RegExp
                  | otherwise = Null

instance ToJSON Literal where
    toJSON lit = object ["value" .= (val :: Text)]
        where val = case lit of
                      StringLit -> "string"
                      BoolLit -> "boolean"
                      Null -> "null"
                      Number -> "number"
                      RegExp -> "RegExp"
