{-# LANGUAGE OverloadedStrings #-}

module JSONflattener
    ( loop
    ) where


import GHC.Generics
import Data.Aeson


{-

eval path = ""


evalH = eval ""
-}

someFunc :: IO ()
someFunc = putStrLn "someFunc"


flatten = id

loop :: IO ()
loop = do
  json <- getContents
  putStr $ flatten json
  loop
