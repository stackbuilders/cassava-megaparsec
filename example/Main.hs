{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Csv (FromRecord, HasHeader (NoHeader))
import Data.Csv.Parser.Megaparsec (decode)
import GHC.Generics (Generic)
import Text.Megaparsec.Error (errorBundlePretty)

data Test = Test {a :: Char, b :: Char, c :: Char} deriving (Eq, Show, Generic)

instance FromRecord Test

main :: IO ()
main = do
  let res = decode @Test NoHeader "example.csv" "a,ba,c\n"
  case res of
    Left errorBundle -> putStrLn $ errorBundlePretty errorBundle
    Right value -> print value
