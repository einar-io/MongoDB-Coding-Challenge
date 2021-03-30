{-# LANGUAGE OverloadedStrings #-}

module JSONflattener
    ( loop
    ) where


import GHC.Exts
import GHC.Generics
import Data.Aeson
import Data.Maybe


import Data.Monoid ((<>))

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.HashMap.Strict as HM

{-
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
-}


--eval :: String -> (String, Value) -> (String,Value)
eval path (k, Object hm) = concatMap (eval (path <> "." <> k)) $ HM.toList hm
eval _    (_k, Array _)  = undefined
eval path (k, v)         = [(path <> k, v)]

evalH :: Value -> Value
evalH (Object hm) = Object $ HM.fromList $ concatMap (eval "") $ HM.toList hm
evalH _ = undefined

-- flatten :: Maybe String -> String
flatten a = T.decodeUtf8 $ T.encodeUtf8 a

loop :: IO ()
loop = do
  input <- T.getContents
  T.putStr $ flatten input
  return ()
