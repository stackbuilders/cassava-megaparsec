{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module IncrementalSpec  where

import           Data.ByteString                        (ByteString)
import           Data.ByteString.Char8                  (pack)
import           Data.Csv                               hiding (NamedRecord)
import           Data.Csv.Incremental                   (HeaderParser (..))
import           Data.Csv.Parser.Megaparsec.Incremental
import           Data.List                              (isInfixOf, uncons)
import qualified Data.Vector                            as V
import qualified Debug.Trace                            as DT
import           GHC.Generics
import           Test.Hspec

spec :: Spec
spec =
  describe "Decoding with headers" decodingHeaderSpec

data Person = Person { name :: String, age :: Int } deriving (Show, Generic)

instance FromNamedRecord Person

dataFailure :: String
dataFailure = unlines
  [ "name,age"
  , "Bart,10"
  , "MrBurns,Unknown"
  , "Lisa,8"
  ]

dataSuccess :: String
dataSuccess = unlines
  [ "name,age"
  , "Bart,10"
  , "Lisa,8"
  , "Maggie,1"
  ]

decodingHeaderSpec :: Spec
decodingHeaderSpec = do
  describe "given csv with malformed input in the middle" $
    it "returns FailH with the uncomsumed input and and error message" $
      case goParser 0 dataFailure (decodeHeader :: HeaderParser (V.Vector Person)) of
        FailH _ errorMsg -> "got \"Unknown\"" `isInfixOf` errorMsg
        _                -> False

  describe "given csv with all right input" $
    it "returns DoneH with the comsumed input" $
      case goParser 0 dataSuccess (decodeHeader :: HeaderParser (V.Vector Person)) of
        DoneH _ r -> V.length (DT.traceShowId r) == 3
        _         -> False

-- | Helpers

-- Parses a single line with a header at a time
feed :: Maybe (String, [String]) -> (ByteString -> a) -> a
feed input f =
  case input of
    Just (firstLine, nextLine:_) -> f (pack $ unlines [firstLine, nextLine])
    Just (_, [])                 -> f mempty
    _                            -> f mempty

-- Parses the given string until the end
goParser :: Int -> String -> HeaderParser a -> HeaderParser a
goParser !acc input (PartialH f) = goParser (acc + 1) input (feed input' f)
  where
    input' =
      case uncons (lines input) of
        Just (fl, nls) -> Just (fl, drop acc nls)
        _              -> Nothing
goParser _ _ r                   = r

