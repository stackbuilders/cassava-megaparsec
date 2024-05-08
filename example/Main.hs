{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Csv hiding (decode, decodeByName, decodeByNameWith, decodeWith)
import Data.Csv.Parser.Megaparsec (decode, decodeByName, decodeByNameWith, decodeWith)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Test = Test {a :: Char, b :: Char, c :: Char} deriving (Eq, Show, Generic)

instance FromRecord Test

main :: IO ()
main = do
  let res = decode @Test NoHeader "example" "a,ba,c\n"
  print res
