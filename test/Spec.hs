{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} 

import Test.Tasty 
import Test.Tasty.HUnit
import Data.Aeson
import Text.RawString.QQ

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E

import JSONflattener

main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000000) tests

tests :: TestTree
tests = testGroup "All"
    [ testCase "Challenge object" $ actual1 @?= expected1
    , testCase "The empty object {}" $ actual2 @?= expected2
    , testCase "The three levels of nesting" $ actual3 @?= expected3
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
actual1 = flattenString test1


-- The handling the empty object
test2:: String
test2 = [r|{}|]

expected2 :: String
expected2 = [r|{}|]

actual2:: String
actual2 = flattenString test2


-- The handling three levels of nesting
test3 :: String
test3 = [r|{"a": {"b": {"c": 1}}}|]

expected3 :: String
expected3 = [r|{
    "a.b.c": 1
}|]

actual3 :: String
actual3 = flattenString test3

