{-# LANGUAGE OverloadedStrings #-}

module APIJson
    where

import Data.Aeson (FromJSON,ToJSON,(.:),object,(.=),(.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>),(<*>),empty, pure)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)

import MozillaApi


writeType val = "type" .= (val :: Text)
object' name = object . ([writeType name] ++)

instance ToJSON SourceLocation
instance ToJSON Position
instance ToJSON ObjectProp

instance ToJSON MemberProp where 
    toJSON (MemId b) = A.toJSON b
    toJSON (MemExpr e) = A.toJSON e


instance ToJSON ObjectKey where 
    toJSON (ObjLit b) = A.toJSON b
    toJSON (ObjId e) = A.toJSON e

instance ToJSON ForDecl where 
    toJSON (ForVar b) = A.toJSON b
    toJSON (ForExpr e) = A.toJSON e

instance ToJSON LambdaBody where 
    toJSON (LBlk b) = A.toJSON b
    toJSON (LExpr e) = A.toJSON e

instance ToJSON LitType where
    toJSON (StringLit str) = A.toJSON str
    toJSON (BoolLit b) = A.toJSON b
    toJSON (NumLit n) = A.toJSON n
    toJSON (Regex) = A.Object H.empty

instance (ToJSON a) => ToJSON (Node a) where
    toJSON (Node x src) = case A.toJSON x of
                            A.Object v -> A.Object (H.insert "loc" (A.toJSON src) v)

instance ToJSON Identifier where
    toJSON (Identifier name) = object' "Identifier" [ "name" .= name ]


instance ToJSON Literal where
    toJSON (Literal lt) = object' "Literal" ["value" .= lt]

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
    toJSON (FunctionDeclaration func) = A.Object (H.insert "type" "FunctionDeclaration" v)
        where (A.Object v) = A.toJSON v
    toJSON (VariableDeclaration dcl) = A.toJSON dcl

instance ToJSON Expression where
    toJSON ThisExpression = object' "ThisExpression" []
    toJSON (ArrayExpression maybe_exps) = object' "ArrayExpression" ["elements" .= maybe_exps]
    toJSON (ObjectExpression props) = object' "ObjectExpression" ["properties" .= props]
    toJSON (FunctionExpression func) = A.Object (H.insert "type" "FunctionExpression" v)
        where (A.Object v) = A.toJSON v
    toJSON (ArrowExpression lambda) = A.Object (H.insert "type" "ArrowExpression" v)
        where (A.Object v) = A.toJSON v
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
                      RRShiftAssign -> ">>>="
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

instance ToJSON Program where
    toJSON (Program body) = object' "Program" ["body" .= body]

------------------------------------------------------------------------

instance FromJSON Position
instance FromJSON SourceLocation
instance FromJSON ObjectProp
instance FromJSON Identifier
instance FromJSON Lambda

parseNode handler (A.Object v)
        | type' == Nothing = empty
        | otherwise = handler (fromJust type') v
        where type' = H.lookup ("type" :: Text) v
parseNode _ _ = empty

instance FromJSON MemberProp where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "Identifier" = MemId <$> A.parseJSON (A.Object v)
                  | otherwise = MemExpr <$> A.parseJSON (A.Object v)

instance FromJSON ObjectKey where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "Identifier" = ObjId <$> A.parseJSON (A.Object v)
                  | type' == "Literal" = ObjLit <$> A.parseJSON (A.Object v)

instance FromJSON ForDecl where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "VariableDeclaration" = ForVar <$> A.parseJSON (A.Object v)
                  | otherwise = ForExpr <$> A.parseJSON (A.Object v)

instance FromJSON LambdaBody where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "BlockStatement" = LBlk <$> A.parseJSON (A.Object v)
                  | otherwise = LExpr <$> A.parseJSON (A.Object v)

instance FromJSON LitType where
    parseJSON val@(A.String _) = StringLit <$> A.parseJSON val
    parseJSON val@(A.Bool _) = BoolLit <$> A.parseJSON val
    parseJSON val@(A.Number _) = NumLit <$> A.parseJSON val
    parseJSON val@(A.Object _) = pure Regex
    parseJSON _ = empty

instance (FromJSON a) => FromJSON (Node a) where
    parseJSON obj@(A.Object v) = Node <$> A.parseJSON obj <*> v .: "loc" 
    parseJSON _ = empty

lookup'' :: [(Text, a)] -> Text -> Maybe a
lookup'' = flip lookup

lookup' lis val = case lookup'' lis val of 
                     Just builder -> builder -- TODO make this safe

instance FromJSON Literal where
    parseJSON (A.Object v) = Literal <$> v .:? "value"
    parseJSON _ = empty


instance FromJSON UnaryOperator where
    parseJSON val@(A.String _) = lookup' lis <$> A.parseJSON val
        where lis = [("-",Negate)
                    ,("+",Positive)
                    ,("!",Bang)
                    ,("~", Tilde)
                    ,("typeof",TypeOf)
                    ,("void",Void)
                    ,("delete",Delete)]
    parseJSON _ = empty

instance FromJSON BinaryOperator where
    parseJSON val@(A.String _) = lookup' lis <$> A.parseJSON val
        where lis = [("==",Equal)
                    ,("!=",NotEqual)
                    ,("===",Same)
                    ,("!==",NotSame)
                    ,("<",MozillaApi.LT)
                    ,("<=",MozillaApi.LTE)
                    ,(">",MozillaApi.GT)
                    ,(">=",MozillaApi.GTE)
                    ,("<<", LShift)
                    ,(">>",RShift)
                    ,(">>>",RRShift)
                    ,("+",Plus)
                    ,("-",Minus)
                    ,("*",Times)
                    ,("/",Div)
                    ,("%",Mod)
                    ,("|",BinOr)
                    ,("^",BinXor)
                    ,("&",BinAnd)
                    ,("in",In)
                    ,("instanceof",InstanceOf)
                    ,("..",DotDot)]
    parseJSON _ = empty

instance FromJSON LogicalOperator where
    parseJSON val@(A.String v) = lookup' lis <$> A.parseJSON val
        where lis = [("||",Or), ("&&",And)]
    parseJSON _ = empty

instance FromJSON AssignmentOperator where
    parseJSON val@(A.String _) = lookup' lis <$> A.parseJSON val
        where lis =  [("=",Assign)
                     ,("+=",PlusAssign)
                     ,("-=",MinusAssign)
                     ,("*=",MultAssign)
                     ,("/=",DivAssign)
                     ,("%=",ModAssign)
                     ,("<<=",LShiftAssign)
                     ,(">>=",RShiftAssign)
                     ,(">>>=", RRShiftAssign)
                     ,("|=",OrAssign)
                     ,("^=",XorAssign)
                     ,("&=",AndAssign)]
    parseJSON _ = empty

instance FromJSON UpdateOperator where
    parseJSON val@(A.String _) = lookup' lis <$> A.parseJSON val
        where lis = [("++",Increment), ("--",Decrement)]
    parseJSON _ = empty


instance FromJSON VariableKind where
    parseJSON val@(A.String _) = lookup' lis <$> A.parseJSON val
        where lis = [("var",Var) ,("let",Let) ,("const",Const)]
    parseJSON _ = empty

instance FromJSON ObjectKind where
    parseJSON val@(A.String v) = makeOp <$> A.parseJSON val
        where makeOp :: Text -> ObjectKind
              makeOp val
                  | val == "init" = Init
                  | val == "get" = Get
                  | val == "set" = Set
    parseJSON _ = empty


instance FromJSON Statement where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "EmptyStatement" = pure EmptyStatement
                  | type' == "ExpressionStatement" = ExpressionStatement <$> v .: "expression"
                  | type' == "BlockStatement" = BlockStatement <$> A.parseJSON (A.Object v)
                  | type' == "IfStatement" = IfStatement
                                             <$> v .: "test"
                                             <*> v .: "consequent"
                                             <*> v .:? "alternate"
                  | type' == "LabeledStatement" = LabeledStatement
                                                  <$> v .: "label"
                                                  <*> v .: "body"
                  | type' == "BreakStatement" = BreakStatement <$> v .:? "label"
                  | type' == "ContinueStatement" = ContinueStatement <$> v .:? "label"
                  | type' == "WithStatement" = WithStatement 
                                               <$> v .: "object"
                                               <*> v .: "body"
                  | type' == "SwitchStatement" = SwitchStatement
                                                 <$> v .: "discriminant"
                                                 <*> v .: "cases"
                                                 <*> v .: "lexical"
                  | type' == "ReturnStatement" = ReturnStatement <$> v .:? "argument"
                  | type' == "ThrowStatement" = ThrowStatement <$> v .: "argument"
                  | type' == "TryStatement" = TryStatement
                                              <$> v .: "block"
                                              <*> v .:? "handler"
                                              <*> v .: "guardedHandlers"
                                              <*> v .:? "finalizer"
                                              
                  | type' == "WhileStatement" = WhileStatement 
                                                <$> v .: "test"
                                                <*> v .: "body"
                  | type' == "DoWhileStatement" = DoWhileStatement
                                                  <$> v .: "body"
                                                  <*> v .: "test"
                  | type' == "ForStatement" = ForStatement
                                              <$> v .:? "init"
                                              <*> v .:? "test"
                                              <*> v .:? "update"
                                              <*> v .: "body"
                  | type' == "ForInStatement" = ForInStatement
                                              <$> v .: "left"
                                              <*> v .: "right"
                                              <*> v .: "body"
                                              <*> v .: "each"
                  | type' == "ForOfStatement" = ForOfStatement
                                                <$> v .: "left"
                                                <*> v .: "right"
                                                <*> v .: "body"
                  | type' == "LetStatement" = LetStatement
                                              <$> v .: "head"
                                              <*> v .: "body"
                  | type' == "DebuggerStatement" = pure DebuggerStatement
                  | type' == "FunctionDeclaration" = FunctionDeclaration
                                                     <$> A.parseJSON (A.Object v)
                  | type' == "VariableDeclaration" = VariableDeclaration
                                                     <$> A.parseJSON (A.Object v)
                  | otherwise = empty

instance FromJSON Expression where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "ThisExpression" = pure ThisExpression
                  | type' == "ArrayExpression" = ArrayExpression <$> v .: "elements"
                  | type' == "ObjectExpression" = ObjectExpression <$> v .: "properties"
                  | type' == "FunctionExpression" = FunctionExpression <$> A.parseJSON (A.Object v)
                  | type' == "ArrowExpression" = ArrayExpression <$> A.parseJSON (A.Object v)
                  | type' == "SequenceExpression" = SequenceExpression <$> v .: "expressions"
                  | type' == "UnaryExpression" = UnaryExpression
                                                 <$> v .: "operator"
                                                 <*> v .: "prefix"
                                                 <*> v .: "argument"
                  | type' == "BinaryExpression" = BinaryExpression
                                                  <$> v .: "operator"
                                                  <*> v .: "left"
                                                  <*> v .: "right"
                  | type' == "AssignmentExpression" = AssignmentExpression
                                                      <$> v .: "operator"
                                                      <*> v .: "left"
                                                      <*> v .: "right"
                  | type' == "UpdateExpression" = UpdateExpression
                                                  <$> v .: "operator"
                                                  <*> v .: "argument"
                                                  <*> v .: "prefix"
                  | type' == "LogicalExpression" = LogicalExpression
                                                   <$> v .: "operator"
                                                   <*> v .: "left"
                                                   <*> v .: "right"
                  | type' == "ConditionalExpression" = ConditionalExpression
                                                       <$> v .: "test"
                                                       <*> v .: "alternate"
                                                       <*> v .: "consequent"
                  | type' == "NewExpression" = NewExpression
                                               <$> v .: "callee"
                                               <*> v .: "arguments"
                  | type' == "CallExpression" = CallExpression
                                                <$> v.: "callee"
                                                <*> v .: "arguments"
                  | type' == "MemberExpression" = MemberExpression
                                                  <$> v .: "object"
                                                  <*> v .: "property"
                                                  <*> v .: "computed"
                  | type' == "YieldExpression" = YieldExpression <$> v .: "argument"
                  | type' == "ComprehensionExpression" = ComprehensionExpression
                                                         <$> v .: "body"
                                                         <*> v .: "blocks"
                                                         <*> v .: "filter"
                  | type' == "GeneratorExpression" = GeneratorExpression
                                                     <$> v .: "body"
                                                     <*> v .: "blocks"
                                                     <*> v .: "filter"
                  | type' == "GraphExpression" = GraphExpression
                                                 <$> v .: "index"
                                                 <*> v .: "expression"
                  | type' == "GraphIndexExpression" = GraphIndexExpression
                                                      <$> v .: "index"
                  | type' == "LetExpression" = LetExpression
                                               <$> v .: "head"
                                               <*> v .: "body"
                  | type' == "Identifier" = IdentifierExpression
                                            <$> A.parseJSON (A.Object v)
                  | type' == "Literal" = LiteralExpression
                                         <$> A.parseJSON (A.Object v)
                  | otherwise = empty

instance FromJSON Pattern where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "ObjectPattern" = ObjectPattern <$> v .: "properties"
                  | type' == "ArrayPattern" = ArrayPattern <$> v .: "elements"
                  | type' == "Identifier" = IdentifierPattern
                                            <$> A.parseJSON (A.Object v)
                  | otherwise = empty

instance FromJSON SwitchCase where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "SwitchCase" = SwitchCase
                                             <$> v .:? "test"
                                             <*> v .: "consequent"
                  | otherwise = empty

instance FromJSON ComprehensionBlock where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "ComprehensionBlock" = ComprehensionBlock
                                                    <$> v .: "left"
                                                    <*> v .: "right"
                                                    <*> v .: "each"
                  | otherwise = empty

instance FromJSON VariableDeclarator where
    parseJSON (A.Object v) = VariableDeclarator <$> v .: "id" <*> v .:? "init"
    parseJSON _ = empty

instance FromJSON VariableDecl where
    parseJSON (A.Object v) = VariableDecl <$> v .: "declarations" <*> v .: "kind"
    parseJSON _ = empty

instance FromJSON Block where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "BlockStatement" = Block  <$> v .: "body"
                  | otherwise = empty

instance FromJSON CatchClause where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "CatchClause" = CatchClause 
                                             <$> v .: "param"
                                             <*> v .: "guard"
                                             <*> v .: "body"
                  | otherwise = empty

instance FromJSON Function where
    parseJSON obj@(A.Object v) = Function
                             <$> v .:? "id"
                             <*> A.parseJSON obj
    parseJSON _ = empty
                             
    
instance FromJSON Program where
    parseJSON = parseNode handler
        where handler type' v
                  | type' == "Program" = Program <$> v .: "body"
                  | otherwise = empty
