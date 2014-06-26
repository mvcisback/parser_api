{-# LANGUAGE DeriveGeneric #-}

module MozillaApi
    where

import Data.Word (Word32)
import Data.Text (Text)
import GHC.Generics (Generic)

type Node a = (a, SourceLocation, Position)

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
                     , expression :: Bool } deriving  (Show, Eq)

data Block =  Block [ Statement ]
           deriving (Show, Eq)


data Statement = EmptyStatement
               | BlockStatement Block
               | ExpressionStatement Expression
               | IfStatement Expression Statement (Maybe Statement)
               | LabeledStatement Identifier Statement
               | BreakStatement (Maybe Identifier)
               | ContinueStatement (Maybe Identifier)
               | WithStatement Expression Statement
               | SwitchStatement Expression [ SwitchCase ] Bool
               | ReturnStatement (Maybe Expression)
               | ThrowStatement Expression
               | TryStatement Block (Maybe CatchClause) [ CatchClause ] (Maybe Block)
               | WhileStatement Expression Statement
               | DoWhileStatement Statement Expression
               | ForStatement (Maybe (Either VariableDecl Expression)) (Maybe Expression) (Maybe Expression) Statement
               | ForInStatement (Either VariableDecl Expression) Expression Statement Bool
               | ForOfStatement (Either VariableDecl Expression) Expression Statement
               | LetStatement [(Pattern, Expression)] Statement
               | DebuggerStatement
               | FunctionDeclaration Function
               | VariableDeclaration VariableDecl
                 deriving (Show, Eq)

data VariableKind = Var | Let | Const deriving (Show, Eq)

data VariableDecl = VariableDecl [VariableDeclarator] VariableKind
                    deriving (Show, Eq)

data VariableDeclarator = VariableDeclarator Pattern (Maybe Expression) deriving (Show, Eq)

data ObjectKind = Init | Get | Set deriving (Show, Eq)
data ObjectProp = ObjectProp {key :: Either Literal Identifier
                             ,value :: Expression
                             ,kind :: ObjectKind}
                deriving (Show,Eq,Generic)

data Expression = ThisExpression
                | ArrayExpression [Maybe Expression]
                | ObjectExpression [ObjectProp]
                | FunctionExpression Function
                | ArrowExpression Lambda
                | SequenceExpression [Expression]
                | UnaryExpression UnaryOperator Bool Expression
                | BinaryExpression BinaryOperator Expression Expression
                | AssignmentExpression AssignmentOperator Expression Expression
                | UpdateExpression UpdateOperator Expression Bool
                | LogicalExpression LogicalOperator Expression Expression
                | ConditionalExpression Expression Expression Expression
                | NewExpression Expression [Expression]
                | CallExpression Expression [Expression]
                | MemberExpression Expression (Either Identifier Expression) Bool
                | YieldExpression (Maybe Expression)
                | ComprehensionExpression Expression [ComprehensionBlock] (Maybe Expression)
                | GeneratorExpression Expression [ComprehensionBlock] (Maybe Expression)
                | GraphExpression Word32 Literal
                | GraphIndexExpression Word32
                | LetExpression [(Pattern, Maybe Expression)] Expression
                | IdentifierExpression Identifier
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
