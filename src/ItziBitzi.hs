{-# LANGUAGE OverloadedStrings #-}

module ItziBitzi
    ( ItziBitzi.interact
    , flatten
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

-- Pretty printer configuration to sort the keys.
cfg :: Config
cfg = defConfig {confCompare = compare}

{- Aeson's datatype for JSON pasted here for convenience:
 
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
-}

-- Adds a dot in the path when we are nested.
suffix :: (Eq a, Data.String.IsString a, Semigroup a) => a -> a -> a
suffix path k = if path == "" then k else path <> "." <> k

-- The object destructuring and key rewrite happens here.
eval :: Text -> (Data.Text.Internal.Text, Value) -> [(Data.Text.Internal.Text, Value)]
eval path (k, Object hm) = concatMap (eval (path `suffix` k)) (HM.toList hm)
eval _    (_k, Array _)  = undefined -- (Not handled according to Assumption 3)
eval path (k, v)         = [(path `suffix` k, v)]

-- The helper function to kick it all off and eventually
-- wrap it all back up in a singleton object.
evalH :: Value -> Value
evalH (Object hm) = Object $ HM.fromList $ reverse $ concatMap (eval "") $ HM.toList hm
evalH _ = undefined

-- The following mostly just changes the representation of the object.
-- It is necessary to get to Aeson's internal representatoin, that we
-- wanted to manipulate in `eval`.
transform :: ByteString -> ByteString
transform = (encodePretty' cfg) . evalH . fromJust . decode

flattenLazy :: LazyText -> LazyText
flattenLazy = E.decodeUtf8 . transform . E.encodeUtf8

flatten :: String -> String
flatten = L.unpack . flattenLazy . L.pack

-- This function returns the object provided on STDIN
-- in its flattened form on STDOUT.
interact :: IO ()
interact = T.interact flattenLazy
