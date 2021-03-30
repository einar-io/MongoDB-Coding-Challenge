{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} 

import Test.Tasty 
import Test.Tasty.HUnit

import Data.Aeson
import Text.RawString.QQ

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E

import ItziBitzi

main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000000) tests

tests :: TestTree
tests = testGroup "All"
    [ testCase "Challenge object" $ actual1 @?= expected1
    , testCase "The empty object {}" $ actual2 @?= expected2
    , testCase "The three levels of nesting" $ actual3 @?= expected3
    , testCase "Two sibling objects" $ actual4 @?= expected4
    , testCase "Handle a `null`" $ actual5 @?= expected5
    , testCase "Clashing keys" $ actual6 @?= expected6
    , testCase "Nested empty object" $ actual7 @?= expected7
  ]

-- The test from the challenge
test1 :: String
test1 = [r|{
    "a": 1,
    "b": true,
    "c": {
        "d": 3,
        "e": "test"
    }
}|]

expected1 :: String
expected1 = [r|{
    "a": 1,
    "b": true,
    "c.d": 3,
    "c.e": "test"
}|]

actual1 :: String
actual1 = flatten test1


-- The handling the empty object
test2:: String
test2 = [r|{}|]

expected2 :: String
expected2 = [r|{}|]

actual2:: String
actual2 = flatten test2


-- The handling three levels of nesting
test3 :: String
test3 = [r|{"a": {"b": {"c": 1}}}|]

expected3 :: String
expected3 = [r|{
    "a.b.c": 1
}|]

actual3 :: String
actual3 = flatten test3


-- Sibling objects
test4 :: String
test4 = [r|{"a": {"aa": 1}, "b": {"bb": 2}}|]

expected4 :: String
expected4 = [r|{
    "a.aa": 1,
    "b.bb": 2
}|]

actual4 :: String
actual4 = flatten test4


-- Null
test5 :: String
test5 = [r|{"Key?": null}|]

expected5 :: String
expected5 = [r|{
    "Key?": null
}|]

actual5 :: String
actual5 = flatten test5


-- Clashing keys.
-- We do not _need_ to handle this pr. the second given assumption.
-- But it serves to document the solution: The latter key-value pair is kept.
test6 :: String
test6 = [r|{"a.aa": 1,  "a": {"aa": 2}}|]

expected6 :: String
expected6 = [r|{
    "a.aa": 2
}|]

actual6 :: String
actual6 = flatten test6


-- "Nested empty object"
test7 :: String
test7 = [r|{"a": {}}|]

expected7 :: String
expected7 = [r|{}|]

actual7 :: String
actual7 = flatten test7

