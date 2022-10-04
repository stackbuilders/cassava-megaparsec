{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module IncrementalSpec (spec) where

import           Data.ByteString.Char8                  (pack)
import           Data.Csv                               hiding (NamedRecord)
import           Data.Csv.Incremental                   (HeaderParser (..))
import           Data.Csv.Parser.Megaparsec.Incremental
import           Data.Vector                            (Vector)
import qualified Debug.Trace                            as DT
import           GHC.Generics
import           GHC.List                               (uncons)
import           Test.Hspec

spec :: Spec
spec =
  describe "Decoding with headers" decodingHeaderSpec

data Person = Person { name :: String, age :: Int } deriving Generic

instance FromNamedRecord Person

dataF = unwords
  [ "name,age"
  , "Bart,10"
  , "MrBurns,Unknown"
  , "Lisa,8"
  ]

feed input n f =
  case nextLine of
    Just value -> f (pack value)
    Nothing    -> f mempty
  where
    remainingLines = drop n (words input)
    nextLine = fst <$> uncons remainingLines

goParser !acc input r@(PartialH f) = goParser (acc + 1) input (feed input acc f)
goParser _ _ r                     = r

decodingHeaderSpec :: Spec
decodingHeaderSpec =
  describe "given csv with malformed input in the middle" $
    fit "returns FailH with the uncomsumed input and and error message" $
      case goParser 0 dataF (decodeHeader :: HeaderParser (Vector Person)) of
        FailH _ errorMsg -> (DT.traceShowId errorMsg) == ""
        _                -> False
