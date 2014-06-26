{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON,ToJSON,(.:),object,(.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>),(<*>),empty)
import Data.Text (Text)

import MozillaApi


writeType val = "type" .= (val :: Text)
object' name = object . ([writeType name] ++)
lambdaFields (Lambda params defaults rest body gen exp) = ["params" .= params
                                                          ,"defaults" .= defaults
                                                          ,"rest" .= rest
                                                          ,"body" .= body
                                                          ,"generator" .= gen
                                                          ,"expression" .= exp]

instance ToJSON SourceLocation
instance ToJSON Position
instance ToJSON ObjectProp

instance ToJSON Identifier where
    toJSON (Identifier name) = object [ "name" .= name ]


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
    toJSON (BlockStatement blk) = A.toJSON blk
    toJSON (ExpressionStatement e) = object' "ExpressionStatement" ["expression" .= e]
    toJSON (IfStatement e s maybe_s2) = object' "IfStatement" ["test" .= e
                                                             , "consequent" .= s
                                                             , "alternate" .= maybe_s2]
    toJSON (LabeledStatement i s) = object' "LabeledStatement" ["label" .= i, "body" .= s]
    toJSON (BreakStatement maybe_i) = object' "BreakStatement"["label" .= maybe_i]
    toJSON (ContinueStatement maybe_i) = object' "ContinueStatement" ["label" .= maybe_i]
    toJSON (WithStatement e s) = object' "WithStatement" ["object" .= e, "body" .= s]
    toJSON (SwitchStatement e cases lex) = object' "SwitchStatement" [ "discriminant" .= e
                                                                     , "cases" .= cases
                                                                     , "lexical" .= lex]
    toJSON (ReturnStatement maybe_e) = object' "ReturnStatement" ["argument" .= maybe_e]
    toJSON (ThrowStatement e) = object' "ThrowStatement" ["argument" .= e]
    toJSON (TryStatement blk cc ccl blk2) = object' "TryStatement" [ "block" .= blk
                                                                  , "handler" .= cc
                                                                  , "guardedHandlers" .= ccl
                                                                  , "finalizer" .= blk2]

    toJSON (WhileStatement e s) = object' "WhileStatement" ["test" .= e, "body" .= s]
    toJSON (DoWhileStatement e s) = object' "DoWhileStatement" ["test" .= e, "body" .= s]
    toJSON (ForStatement init test update body) = object' "ForStatement" ["init" .= init
                                                                         ,"test" .= test
                                                                         ,"update" .= update
                                                                         ,"body" .= body]
    toJSON (ForInStatement lft rgt body each) = object' "ForInStatement" ["left" .= lft
                                                                         ,"right" .= rgt
                                                                         ,"body" .= body
                                                                         ,"each" .= each]
    toJSON (ForOfStatement lft rgt body) = object' "ForOfStatement" ["left" .= lft
                                                                    ,"right" .= rgt
                                                                    ,"body" .= body]
    toJSON (LetStatement head body) = object' "LetStatement" [ "head" .= head
                                                             , "body" .= body]
    toJSON DebuggerStatement = object' "DebuggerStatement" []
    toJSON (FunctionDeclaration (Function name lambda)) = object' "FunctionDeclaration" fields
        where fields = ("name" .= name):(lambdaFields lambda)
    toJSON (VariableDeclaration dcl) = A.toJSON dcl

instance ToJSON Expression where
    toJSON ThisExpression = object' "ThisExpression" []
    toJSON (ArrayExpression maybe_exps) = object' "ArrayExpression" ["elements" .= maybe_exps]
    toJSON (ObjectExpression props) = object' "ObjectExpression" ["properties" .= props]
    toJSON (FunctionExpression (Function name lambda)) = object' "FunctionExpression" fields
        where fields = ("name" .= name):(lambdaFields lambda)
    toJSON (ArrowExpression lambda) = object' "ArrowExpression" (lambdaFields lambda)
    toJSON (SequenceExpression exps) = object' "SequenceExpression" ["expressions" .= exps]
    toJSON (UnaryExpression op prefix arg) = object' "UnaryExpression" [ "operator" .= op
                                                                       , "prefix" .= prefix
                                                                       , "argument" .= arg]
    toJSON (BinaryExpression op lft rgt) = object' "BinaryExpression" [ "operator" .= op
                                                                      , "left" .= lft
                                                                      , "right" .= rgt]
    toJSON (AssignmentExpression op lft rgt) = object' "AssignmentExpression" [ "operator" .= op
                                                                              , "left" .= lft
                                                                              , "right" .= rgt]
    toJSON (UpdateExpression op arg prefix) = object' "UpdateExpression" [ "operator" .= op
                                                                         , "argument" .= arg
                                                                         , "prefix" .= prefix]
    toJSON (LogicalExpression op lft rgt) = object' "LogicalExpression" [ "operator" .= op
                                                                        , "left" .= lft
                                                                        , "right" .= rgt]

                                             
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
    toJSON (ComprehensionExpression body blocks filter) = object' "ComprehensionExpression" [ "body" .= body
                                                                                            , "blocks" .= blocks
                                                                                            , "filter" .= filter]
    toJSON (GeneratorExpression body blocks filter) = object' "GeneratorExpression" ["body" .= body
                                                                                    ,"blocks" .= blocks
                                                                                    ,"filter" .= filter]
    toJSON (GraphExpression i lit) = object' "GraphExpression" ["index" .= i, "expression" .= lit]
    toJSON (GraphIndexExpression i) = object' "GraphIndexExpression" ["index" .= i]
    toJSON (LetExpression head body) = object' "LetExpression" ["head" .= head, "body" .= body]
    toJSON (IdentifierExpression i) = A.toJSON i

instance ToJSON Pattern where
    toJSON (ObjectPattern props) = object' "ObjectPattern" ["properties" .= props]
    toJSON (ArrayPattern elements) = object' "ArrayPattern" ["elements" .= elements]
    toJSON (IdentifierPattern i) = A.toJSON i

instance ToJSON SwitchCase where
    toJSON (SwitchCase test sl) = object' "SwitchCase" ["test" .= test, "consequent" .= sl]


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

instance ToJSON VariableDeclarator where
    toJSON (VariableDeclarator ident init) = object' "VariableDeclarator" ["id" .= ident, "init" .= init]

instance ToJSON VariableKind where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Var -> "var"
                      Let -> "let"
                      Const -> "const"

instance ToJSON VariableDecl where
    toJSON (VariableDecl decls kind) = object' "VariableDeclaration" ["declarations" .= decls, "kind" .= kind]

instance ToJSON ObjectKind where
    toJSON val = A.toJSON (str :: Text)
        where str = case val of
                      Init -> "init"
                      Get -> "get"
                      Set -> "set"

instance ToJSON Block where
    toJSON (Block stmts) = object' "BlockStatement" ["body" .= stmts]

instance ToJSON CatchClause where
    toJSON (CatchClause param guard body) = object' "CatchClause" ["param" .= param
                                                                  ,"guard" .= guard
                                                                  ,"body" .= body]
instance ToJSON ComprehensionBlock where
    toJSON (ComprehensionBlock lft rgt each) = object' "ComprehensionBlock" ["left" .= lft
                                                                            ,"right" .= rgt
                                                                            ,"each" .= each]


------------------------------------------------------------------------

instance FromJSON Position
instance FromJSON SourceLocation

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

instance FromJSON Identifier where
    parseJSON (A.Object v) = Identifier <$> v .: "name"
    parseJSON _ = empty
