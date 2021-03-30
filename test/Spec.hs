{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} 

import Test.Tasty 
import Test.Tasty.HUnit
import Data.Aeson

import JSONflattener

import Data.Text.Internal (showText)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import Text.RawString.QQ

main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000000) tests

tests :: TestTree
tests = testGroup "All"
    [ testCase "Challenge" $ actual1 @?= expected1 ]

test1:: String
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

actual1:: String
actual1 = showText $ T.toStrict $ flatten $ T.pack test1



