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
  , "Lisa,8"
  , "MrBurns,Unknown"
  ]

dataSuccess :: String
dataSuccess = unlines
  [ "name,age"
  , "Bart,10"
  , "Lisa,8"
  , "Maggie,1"
  ]

runParserWithSteps :: String -> (HeaderParser (V.Vector Person), Int)
runParserWithSteps text =
  goParser 0 text decodeHeader

decodingHeaderSpec :: Spec
decodingHeaderSpec = do
  describe "given csv with malformed input in the middle" $ do
    it "returns FailH with the uncomsumed input and error message" $
      case runParserWithSteps dataFailure of
        (FailH _ errorMsg, _) -> "got \"Unknown\"" `isInfixOf` errorMsg
        _                     -> False

    it "performs three steps (2 partials + 1 fail) before failing" $
      case runParserWithSteps dataFailure of
        (FailH _ _, steps) -> steps == 3
        _                  -> False

  describe "given csv with all right input" $ do
    it "returns DoneH with the comsumed input" $
      case runParserWithSteps dataSuccess of
        (DoneH h r, _) -> V.length r == 3
        _              -> False

    it "performs four steps (3 partial + 1 done) before finishing" $
      case runParserWithSteps dataSuccess of
        (DoneH _ _, steps) -> steps == 4
        _                  -> False


-- | Helpers

-- Parses a single line with a header at a time
feed :: Maybe (String, [String]) -> (ByteString -> HeaderParser (V.Vector a)) -> HeaderParser (V.Vector a)
feed input f =
  case input of
    Just (firstLine, nextLine:_) -> f (pack $ unlines [firstLine, nextLine])
    Just (_, [])                 -> DoneH mempty V.empty
    _                            -> DoneH mempty V.empty

-- Parses the given string until the end
goParser :: Int -> String -> HeaderParser (V.Vector a) -> (HeaderParser (V.Vector a), Int)
goParser !acc input (PartialH f) =
  goParser (acc + 1) input (feed input' f)
    where
      input' =
        case uncons (lines input) of
          Just (fl, nls) -> Just (fl, drop acc nls)
          _              -> Nothing
goParser acc _ r                   = (r, acc)
