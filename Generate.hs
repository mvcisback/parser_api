{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Generate 
    where

import Prelude hiding (GT, LT)

import Data.DeriveTH
import Data.Text

import Test.QuickCheck hiding (Positive)
import MozillaApi

instance Arbitrary Text where
    arbitrary = elements ["x", "y", "z"]

$( derive makeArbitrary ''UpdateOperator )
$( derive makeArbitrary ''AssignmentOperator )
$( derive makeArbitrary ''LogicalOperator )
$( derive makeArbitrary ''UnaryOperator )
$( derive makeArbitrary ''BinaryOperator )
$( derive makeArbitrary ''VariableKind )
$( derive makeArbitrary ''ObjectKind )
$( derive makeArbitrary ''SourceLocation )
$( derive makeArbitrary ''Position )
$( derive makeArbitrary ''Identifier )
$( derive makeArbitrary ''Literal )
$( derive makeArbitrary ''LitType )
$( derive makeArbitrary ''Node )
$( derive makeArbitrary ''Function )
$( derive makeArbitrary ''Program )
$( derive makeArbitrary ''LambdaBody )
$( derive makeArbitrary ''Lambda )
$( derive makeArbitrary ''Block )
$( derive makeArbitrary ''ForDecl )
-- $( derive makeArbitrary ''Statement )
$( derive makeArbitrary ''VariableDecl )
$( derive makeArbitrary ''VariableDeclarator )
$( derive makeArbitrary ''ObjectKey )
$( derive makeArbitrary ''ObjectProp )
$( derive makeArbitrary ''MemberProp )
-- $( derive makeArbitrary ''Expression )
-- $( derive makeArbitrary ''Pattern )
$( derive makeArbitrary ''SwitchCase )
$( derive makeArbitrary ''CatchClause )
$( derive makeArbitrary ''ComprehensionBlock )


arbExp =
   do x <- choose (0 :: Int, 22)
      case x of
          0 -> return ThisExpression
          1 -> do x1 <- arbitrary
                  return (ArrayExpression x1)
          2 -> do x1 <- arbitrary
                  return (ObjectExpression x1)
          3 -> do x1 <- arbitrary
                  return (FunctionExpression x1)
          4 -> do x1 <- arbitrary
                  return (ArrowExpression x1)
          5 -> do x1 <- arbitrary
                  return (SequenceExpression x1)
          6 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (UnaryExpression x1 x2 x3)
          7 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (BinaryExpression x1 x2 x3)
          8 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (AssignmentExpression x1 x2 x3)
          9 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (UpdateExpression x1 x2 x3)
          10 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (LogicalExpression x1 x2 x3)
          11 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (ConditionalExpression x1 x2 x3)
          12 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (NewExpression x1 x2)
          13 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (CallExpression x1 x2)
          14 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (MemberExpression x1 x2 x3)
          15 -> do x1 <- arbitrary
                   return (YieldExpression x1)
          16 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (ComprehensionExpression x1 x2 x3)
          17 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (GeneratorExpression x1 x2 x3)
          18 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (GraphExpression x1 x2)
          19 -> do x1 <- arbitrary
                   return (GraphIndexExpression x1)
          20 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (LetExpression x1 x2)
          21 -> do x1 <- arbitrary
                   return (IdentifierExpression x1)
          22 -> do x1 <- arbitrary
                   return (LiteralExpression x1)
          _ -> error "FATAL ERROR: Arbitrary instance, logic bug"
 

arbStmt = 
   do x <- choose (0 :: Int, 20)
      case x of
          0 -> return EmptyStatement
          1 -> do x1 <- arbitrary
                  return (BlockStatement x1)
          2 -> do x1 <- arbitrary
                  return (ExpressionStatement x1)
          3 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (IfStatement x1 x2 x3)
          4 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  return (LabeledStatement x1 x2)
          5 -> do x1 <- arbitrary
                  return (BreakStatement x1)
          6 -> do x1 <- arbitrary
                  return (ContinueStatement x1)
          7 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  return (WithStatement x1 x2)
          8 -> do x1 <- arbitrary
                  x2 <- arbitrary
                  x3 <- arbitrary
                  return (SwitchStatement x1 x2 x3)
          9 -> do x1 <- arbitrary
                  return (ReturnStatement x1)
          10 -> do x1 <- arbitrary
                   return (ThrowStatement x1)
          11 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   x4 <- arbitrary
                   return (TryStatement x1 x2 x3 x4)
          12 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (WhileStatement x1 x2)
          13 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (DoWhileStatement x1 x2)
          14 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   x4 <- arbitrary
                   return (ForStatement x1 x2 x3 x4)
          15 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   x4 <- arbitrary
                   return (ForInStatement x1 x2 x3 x4)
          16 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   x3 <- arbitrary
                   return (ForOfStatement x1 x2 x3)
          17 -> do x1 <- arbitrary
                   x2 <- arbitrary
                   return (LetStatement x1 x2)
          18 -> return DebuggerStatement
          19 -> do x1 <- arbitrary
                   return (FunctionDeclaration x1)
          20 -> do x1 <- arbitrary
                   return (VariableDeclaration x1)
          _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

arbPat = do x <- choose (0 :: Int, 2)
            case x of
              0 -> do x1 <- arbitrary
                      return (ObjectPattern x1)
              1 -> do x1 <- arbitrary
                      return (ArrayPattern x1)
              2 -> do x1 <- arbitrary
                      return (IdentifierPattern x1)
              _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary Pattern where
    arbitrary = frequency [(1, arbPat)
                          ,(100, do x1 <- arbitrary
                                    return (IdentifierPattern x1))]
          

instance Arbitrary Statement where
    arbitrary = frequency [(1, arbStmt)
                          ,(20, return EmptyStatement :: Gen Statement)]


instance Arbitrary Expression where
    arbitrary = frequency [(20, return ThisExpression :: Gen Expression)
                          ,(1, arbExp)]

