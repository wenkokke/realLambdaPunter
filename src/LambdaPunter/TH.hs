module LambdaPunter.TH where

import Data.Char (isLower,toLower)

dropFirstWord :: String -> String
dropFirstWord lbl = let (x:xs) = dropWhile isLower lbl in toLower x : xs
