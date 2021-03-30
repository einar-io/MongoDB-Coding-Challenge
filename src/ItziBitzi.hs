{-# LANGUAGE OverloadedStrings #-}

module ItziBitzi
    ( loop
    , flatten
    , flattenString
    ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.String
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Internal (Text)
import qualified Data.Text.Internal.Lazy (Text)

type Text = Data.Text.Internal.Text
type LazyText = Data.Text.Internal.Lazy.Text

-- Sort the keys
cfg :: Config
cfg = defConfig {confCompare = compare}

{-
 - Aeson's datatype for JSON:
 -
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
-}

suffix :: (Eq a, Data.String.IsString a, Semigroup a) => a -> a -> a
suffix path k = if path == "" then k else path <> "." <> k

eval :: Text -> (Data.Text.Internal.Text, Value) -> [(Data.Text.Internal.Text, Value)]
eval path (k, Object hm) = concatMap (eval (path `suffix` k)) (HM.toList hm)
eval _    (_k, Array _)  = undefined
eval path (k, v)         = [(path `suffix` k, v)]

evalH :: Value -> Value
evalH (Object hm) = Object $ HM.fromList $ reverse $ concatMap (eval "") $ HM.toList hm
evalH _ = undefined

transform :: ByteString -> ByteString
transform = (encodePretty' cfg) . evalH . fromJust . decode

flatten :: LazyText -> LazyText
flatten = E.decodeUtf8 . transform . E.encodeUtf8

flattenString :: String -> String
flattenString = L.unpack . flatten . L.pack

loop :: IO ()
loop = T.interact flatten

