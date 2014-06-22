-- TODO: Change some of the Int -> Data.Word.Word32 if its supposed to be uint32

data SourceLocation = SourceLocation { source :: Maybe String
                                     , start :: Int
                                     , end :: Int
                                     } deriving (Show, Eq, Ord)

data Position = Position { line :: Int
                         , column :: Int
                         } deriving (Show, Eq, Ord)

data Function = Function (Maybe Identifier) Lambda
               deriving  (Show, Eq, Ord)

data Program = Program [ Statement ]
               deriving  (Show, Eq, Ord)

data Lambda = Lambda {params :: [ Pattern ]
                     , defaults :: [ Expression ]
                     , rest :: Maybe Identifier
                     , body :: Either Block Expression
                     , generator :: Bool
                     , expression :: Bool } deriving  (Show, Eq, Ord)

data Block =  Block [ Statement ]
           deriving (Show, Eq, Ord)
data MaybeEither a b = First a | Second b | Nothing
                   deriving (Show, Eq, Ord)

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
               | ForStatement (MaybeEither VariableDecl Expression) (Maybe Expression) (Maybe Expression) Statement
               | ForInStatement (Either VariableDecl Expression) Expression Statement Bool
               | ForOfStatement (Either VariableDecl Expression) Expression Statement
               | LetStatement [(Pattern, Expression)] Statement
               | DebuggerStatement
               | FunctionDeclaration Function
               | VariableDeclaration VariableDecl
                 deriving (Show, Eq, Ord)

data VariableKind = Var | Let | Const deriving (Show, Eq, Ord)

data VariableDecl = VariableDecl [VariableDeclarator] VariableKind
                    deriving (Show, Eq, Ord)

data VariableDeclarator = VariableDeclarator Pattern (Maybe Expression) deriving (Show, Eq, Ord)

data ObjectKind = Init | Get | Set deriving (Show, Eq, Ord)

data Expression = ThisExpression
                | ArrayExpression [Maybe Expression]
                | ObjectExpression [(Either Literal Identifier, Expression, ObjectKind)]
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
                | GeneratorExpression [ComprehensionBlock] (Maybe Expression)
                | GraphExpression Int Literal
                | GraphIndexExpression Int
                | LetExpression [(Pattern, Maybe Expression)] Expression
                | IdentifierExpression Identifier
                  deriving (Show, Eq, Ord)

data Pattern = Object [(Either Literal Identifier,Pattern)]
             | ArrayPattern [Maybe Pattern]
             | IdentifierPattern Identifier
               deriving (Show, Eq, Ord)

data SwitchCase = SwitchCase (Maybe Expression) [Statement]
                  deriving (Show, Eq, Ord)

data CatchClause = CatchClause Pattern (Maybe Expression) Block
                   deriving (Show, Eq, Ord)

data ComprehensionBlock = ComprehensionBlock Pattern Expression Bool
                          deriving (Show, Eq, Ord)

                        
type Identifier = String
data Literal = StringLit | BoolLit | Null | Number | RegExp
             deriving (Show, Eq, Ord)

data UnaryOperator = Minus | Plus | Bang | Tilde | TypeOf | Void | Delete
                   deriving (Show, Eq, Ord)

type BinaryOperator = String -- TODO enumerate the various operators....

data LogicalOperator = Or | And
                     deriving (Show, Eq, Ord)

type AssignmentOperator = String -- TODO enumerate the various operators....

data UpdateOperator = Increment | Decrement
                    deriving (Show, Eq, Ord)

