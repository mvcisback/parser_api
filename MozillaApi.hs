{-# LANGUAGE DeriveGeneric #-}

module MozillaApi
    where

import Data.Word (Word32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Node a = Node a SourceLocation
              deriving (Show, Eq, Generic)
type ExprNode = Node Expression
type StateNode = Node Statement
type BlkNode = Node Block
type IdNode = Node Identifier
type PatNode = Node Pattern

data SourceLocation = SourceLocation { source :: Maybe Text
                                     , start :: Position
                                     , end :: Position
                                     } deriving (Show,Eq,Generic)

data Position = Position { line :: Word32 -- TODO add the >= 1 to the type
                         , column :: Word32
                         } deriving (Show,Eq,Generic)

newtype Identifier = Identifier Text
    deriving (Show,Eq)

data Literal = StringLit | BoolLit | Null | Number | RegExp
             deriving (Show, Eq)

data Function = Function (Maybe Identifier) Lambda
               deriving  (Show, Eq)

data Program = Program [ Statement ]
               deriving  (Show, Eq)

data Lambda = Lambda {params :: [ Pattern ]
                     , defaults :: [ Expression ]
                     , rest :: Maybe Identifier
                     , body :: Either Block Expression
                     , generator :: Bool
                     , expression :: Bool } deriving  (Show, Eq, Generic)

data Block =  Block [ Statement ]
           deriving (Show, Eq)


data Statement = EmptyStatement 
               | BlockStatement BlkNode
               | ExpressionStatement ExprNode
               | IfStatement ExprNode StateNode (Maybe StateNode)
               | LabeledStatement IdNode StateNode
               | BreakStatement (Maybe IdNode)
               | ContinueStatement (Maybe IdNode)
               | WithStatement ExprNode StateNode
               | SwitchStatement ExprNode [ (Node SwitchCase) ] Bool
               | ReturnStatement (Maybe ExprNode)
               | ThrowStatement ExprNode
               | TryStatement BlkNode (Maybe (Node CatchClause)) [ (Node CatchClause) ] (Maybe BlkNode)
               | WhileStatement ExprNode StateNode
               | DoWhileStatement StateNode ExprNode
               | ForStatement ((Maybe (Either (Node VariableDecl) ExprNode))) (Maybe ExprNode) (Maybe ExprNode) StateNode
               | ForInStatement (Either (Node VariableDecl) ExprNode) ExprNode StateNode Bool
               | ForOfStatement (Either (Node VariableDecl) ExprNode) ExprNode StateNode
               | LetStatement [(PatNode, ExprNode)] StateNode
               | DebuggerStatement
               | FunctionDeclaration (Node Function)
               | VariableDeclaration (Node VariableDecl)
                 deriving (Show, Eq)

data VariableKind = Var | Let | Const deriving (Show, Eq)

data VariableDecl = VariableDecl [Node VariableDeclarator] VariableKind
                    deriving (Show, Eq)

data VariableDeclarator = VariableDeclarator PatNode (Maybe ExprNode) deriving (Show, Eq)

data ObjectKind = Init | Get | Set deriving (Show, Eq)
data ObjectProp = ObjectProp {key :: Either (Node Literal) IdNode
                             ,value :: ExprNode
                             ,kind :: ObjectKind}
                deriving (Show,Eq,Generic)

data Expression = ThisExpression
                | ArrayExpression [Maybe ExprNode]
                | ObjectExpression [ObjectProp]
                | FunctionExpression (Node Function)
                | ArrowExpression (Node Lambda)
                | SequenceExpression [ExprNode]
                | UnaryExpression UnaryOperator Bool ExprNode
                | BinaryExpression BinaryOperator ExprNode ExprNode
                | AssignmentExpression AssignmentOperator ExprNode ExprNode
                | UpdateExpression UpdateOperator ExprNode Bool
                | LogicalExpression LogicalOperator ExprNode ExprNode
                | ConditionalExpression ExprNode ExprNode ExprNode
                | NewExpression ExprNode [ExprNode]
                | CallExpression ExprNode [ExprNode]
                | MemberExpression ExprNode (Either IdNode ExprNode) Bool
                | YieldExpression (Maybe ExprNode)
                | ComprehensionExpression ExprNode [Node ComprehensionBlock] (Maybe ExprNode)
                | GeneratorExpression ExprNode [Node ComprehensionBlock] (Maybe ExprNode)
                | GraphExpression Word32 (Node Literal)
                | GraphIndexExpression Word32
                | LetExpression [(PatNode, Maybe ExprNode)] ExprNode
                | IdentifierExpression IdNode
                | LiteralExpression (Node Literal)
                  deriving (Show, Eq)

data Pattern = ObjectPattern [(Either Literal Identifier,Pattern)]
             | ArrayPattern [Maybe Pattern]
             | IdentifierPattern Identifier
               deriving (Show, Eq)

data SwitchCase = SwitchCase (Maybe Expression) [Statement]
                  deriving (Show, Eq)

data CatchClause = CatchClause Pattern (Maybe Expression) Block
                   deriving (Show, Eq)

data ComprehensionBlock = ComprehensionBlock Pattern Expression Bool
                          deriving (Show, Eq)
                        

data UnaryOperator = Negate | Positive | Bang | Tilde | TypeOf | Void | Delete
                     deriving (Show, Eq)

data BinaryOperator = Equal | NotEqual | Same | NotSame | LT | LTE | GT | GTE 
                    | LShift | RShift | RRShift | Plus | Minus | Times | Div 
                    | Mod | BinOr | BinXor | BinAnd | In | InstanceOf | DotDot
                      deriving (Show, Eq)

data LogicalOperator = Or | And
                     deriving (Show, Eq)

data AssignmentOperator = Assign | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign
                        | LShiftAssign | RShiftAssign | RRShiftAssign | OrAssign | XorAssign
                        | AndAssign
                          deriving (Show, Eq)

data UpdateOperator = Increment | Decrement
                    deriving (Show, Eq)
