{-# LANGUAGE OverloadedStrings #-}

module JSONflattener
    ( loop
    , flatten
    ) where

import Data.Aeson
import Data.Maybe

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Internal (Text)
import qualified Data.Text.Internal.Lazy (Text)

type Text = Data.Text.Internal.Text
type LazyText = Data.Text.Internal.Lazy.Text
{-
 - Aeson's datatype:
 -
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
-}

suffix path k = if path == "" then k else path <> "." <> k <> "."

eval :: Text -> (Data.Text.Internal.Text, Value) -> [(Data.Text.Internal.Text, Value)]
eval path (k, Object hm) = concatMap (eval (path `suffix` k)) (HM.toList hm)
eval _    (_k, Array _)  = undefined
eval path (k, v)         = [(path <> k, v)]

evalH :: Value -> Value
evalH (Object hm) = Object $ HM.fromList $ reverse $ concatMap (eval "") $ HM.toList hm
evalH _ = undefined

transform :: ByteString -> ByteString
transform = encode . evalH . fromJust . decode

flatten :: LazyText -> LazyText
flatten = T.decodeUtf8 . transform . T.encodeUtf8

{-
loop :: IO ()
loop = do
  input <- getContents
  T.putStr $ flatten input
-}

loop :: IO ()
loop = T.interact flatten
