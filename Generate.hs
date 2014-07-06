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
$( derive makeArbitrary ''Statement )
$( derive makeArbitrary ''VariableDecl )
$( derive makeArbitrary ''VariableDeclarator )
$( derive makeArbitrary ''ObjectKey )
$( derive makeArbitrary ''ObjectProp )
$( derive makeArbitrary ''MemberProp )
$( derive makeArbitrary ''Expression )
$( derive makeArbitrary ''Pattern )
$( derive makeArbitrary ''SwitchCase )
$( derive makeArbitrary ''CatchClause )
$( derive makeArbitrary ''ComprehensionBlock )

