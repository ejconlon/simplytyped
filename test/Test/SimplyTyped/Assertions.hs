module Test.SimplyTyped.Assertions where

import Control.Monad (unless)
import SimplyTyped.Prelude
import Test.Tasty.HUnit

assertPredicate :: (Show a, HasCallStack) => (a -> a -> Bool) -> String -> a -> a -> Assertion
assertPredicate predicate preface left right = unless (predicate left right) (assertFailure msg)
  where
    msg =
      (if null preface
         then ""
         else preface ++ "\n") ++
      "left: " ++ show left ++ "\n right: " ++ show right

assertNotEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
assertNotEqual = assertPredicate (/=) "Expected not equal"

assertTrue :: HasCallStack => Bool -> Assertion
assertTrue a = assertPredicate (==) "Expected True" a True

assertFalse :: HasCallStack => Bool -> Assertion
assertFalse a = assertPredicate (==) "Expected False" a False

(@/=) :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
(@/=) = assertNotEqual
