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

writeType val = "type" .= (val :: Text)
object' name = object . ([writeType name] ++)

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
    parseJSON _ = empty


instance ToJSON Literal where
    toJSON lit = object ["value" .= (val :: Text)]
        where val = case lit of
                      StringLit -> "string"
                      BoolLit -> "boolean"
                      Null -> "null"
                      Number -> "number"
                      RegExp -> "RegExp"

instance ToJSON Statement where
    toJSON EmptyStatement = object' "EmptyStatement" []
    toJSON (ExpressionStatement e) = object' "ExpressionStatement" ["expression" .= e]
    toJSON (IfStatement e s maybe_s2) = object' "IfStatement" ["test" .= e
                                                             , "consequent" .= s
                                                             , "alternate" .= maybe_s2]
    toJSON (LabeledStatement i s) = object' "LabeledStatement" ["label" .= i, "body" .= s]
    toJSON (BreakStatement maybe_i) = object' "BreakStatement"["label" .= maybe_i]
    toJSON (ContinueStatement maybe_i) = object' "ContinueStatement" ["label" .= maybe_i]
    toJSON (WithStatement e s) = object' "WithStatement" ["object" .= e, "body" .= s]
    toJSON (ReturnStatement maybe_e) = object' "ReturnStatement" ["argument" .= maybe_e]
    toJSON (ThrowStatement e) = object' "ThrowStatement" ["argument" .= e]
    toJSON (WhileStatement e s) = object' "WhileStatement" ["test" .= e, "body" .= s]
    toJSON (DoWhileStatement e s) = object' "DoWhileStatement" ["test" .= e, "body" .= s]
    toJSON DebuggerStatement = object' "DebuggerStatement" []


instance ToJSON Expression where
    toJSON ThisExpression = object' "ThisExpression" []
    toJSON (ArrayExpression maybe_exps) = object' "ArrayExpression" ["elements" .= maybe_exps]
    toJSON (SequenceExpression exps) = object' "SequenceExpression" ["expressions" .= exps]
    toJSON (ConditionalExpression e1 e2 e3) = object' "ConditionalExpression" ["test" .= e1
                                                                              , "consequent" .= e2
                                                                              , "alternate" .= e3
                                                                              ]
    toJSON (NewExpression e el) = object' "NewExpression" ["callee" .= e, "arguments" .= el]
    toJSON (CallExpression e el) = object' "CallExpression" ["callee" .= e, "arguments" .= el]
    toJSON (MemberExpression e prop computed) = object' "MemberExpression" ["object" .= e
                                                                           , "property" .= prop
                                                                           , "computed" .= computed
                                                                           ]
    toJSON (YieldExpression arg) = object' "YieldExpression" ["argument" .= arg]
    toJSON (GraphExpression i lit) = object' "GraphExpression" ["index" .= i, "expression" .= lit]
    toJSON (GraphIndexExpression i) = object' "GraphIndexExpression" ["index" .= i]
    toJSON (IdentifierExpression i) = A.toJSON i

instance ToJSON Pattern where
    toJSON (ObjectPattern props) = object' "ObjectPattern" ["properties" .= props]
    toJSON (ArrayPattern elements) = object' "ArrayPattern" ["elements" .= elements]
    toJSON (IdentifierPattern i) = A.toJSON i

instance ToJSON SwitchCase where
    toJSON (SwitchCase test sl) = object' "SwitchCase" ["test" .= test, "consequent" .= sl]

instance ToJSON ComprehensionBlock where
    toJSON (ComprehensionBlock left right each) = object' "ComprehensionBlock" [ "left" .= left
                                                                               , "right" .= right
                                                                               , "each" .= each
                                                                               ]
instance ToJSON UnaryOperator where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Negate -> "-"
                      Positive -> "+"
                      Bang -> "!"
                      Tilde -> "~"
                      TypeOf -> "typeof"
                      Void -> "void"
                      Delete -> "delete"

instance ToJSON BinaryOperator where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Equal -> "=="
                      NotEqual -> "!="
                      Same -> "==="
                      NotSame -> "!=="
                      MozillaApi.LT -> "<"
                      MozillaApi.LTE -> "<="
                      MozillaApi.GT -> ">"    
                      MozillaApi.GTE -> ">="
                      LShift -> "<<"
                      RShift -> ">>"
                      RRShift -> ">>>"
                      Plus -> "+"
                      Minus -> "-"
                      Times -> "*"
                      Div -> "/"
                      Mod -> "%"
                      BinOr -> "|"
                      BinXor -> "^"
                      BinAnd -> "&"
                      In -> "in"
                      InstanceOf -> "instanceof"
                      DotDot -> ".."

instance ToJSON LogicalOperator where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Or -> "||"
                      And -> "&&"

instance ToJSON AssignmentOperator where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Assign -> "="
                      PlusAssign -> "+="
                      MinusAssign -> "-="
                      MultAssign -> "*="
                      DivAssign -> "/="
                      ModAssign -> "%="
                      LShiftAssign -> "<<="
                      RShiftAssign -> ">>="
                      RRShiftAssign -> ">>="
                      OrAssign -> "|="
                      XorAssign -> "^="
                      AndAssign -> "&="

instance ToJSON UpdateOperator where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Increment -> "++"
                      Decrement -> "--"
