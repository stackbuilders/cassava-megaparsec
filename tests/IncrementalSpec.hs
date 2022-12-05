{-# LANGUAGE DeriveGeneric #-}

module IncrementalSpec  where

import           Data.ByteString.Lazy                   (ByteString, drop, take)
import           Data.ByteString.Lazy.Char8             (pack)
import           Data.Csv                               (FromRecord)
import           Data.Csv.Parser.Megaparsec.Incremental (Parser (..), decode)
import qualified Data.Csv.Parser.Megaparsec.Internals   as I
import           Data.Either                            (isRight)
import           Data.List                              (isInfixOf, uncons)
import qualified Data.Vector                            as V
import qualified Debug.Trace                            as DT
import           GHC.Generics
import           Prelude                                hiding (drop, take)
import           Test.Hspec
import           Text.Megaparsec                        (ParseErrorBundle)

spec :: Spec
spec =
  describe "Decoding without headers" decodingSpec

data Person = Person { name :: String, age :: Int } deriving (Show, Generic)

instance FromRecord Person

dataFailure :: ByteString
dataFailure = pack $ unlines
  [ "name,age"
  , "Bart,10"
  , "MrBurns,Unknown"
  , "MoleMan,Unknown"
  , "Lisa,8"
  ]

dataSuccess :: ByteString
dataSuccess = pack $ unlines
  [ "name,age"
  , "Bart,10"
  , "Lisa,8"
  , "Maggie,1"
  ]

decodingSpec :: Spec
decodingSpec = do
  describe "given csv with wrong input in the middle" $ do
    it "parses all the file and returns errors and " $
      case DT.traceShowId $ V.break isRight (parseAccumulator dataFailure) of
        (errors, successes) ->
          V.length errors == 2
          && V.length successes == 2

    -- it "performs three steps (2 partials + 1 fail) before failing" $
      -- case runParserWithSteps dataFailure of
      --   (Fail _ _, steps) -> steps == 3
      --   _                  -> False

  -- describe "given csv with all right input" $ do
    -- it "returns DoneH with the comsumed input" $
      -- case runParserWithSteps dataSuccess of
      --   (Done h r, _) -> V.length r == 3
      --   _              -> False

    -- it "performs four steps (3 partial + 1 done) before finishing" $
      -- case runParserWithSteps dataSuccess of
      --   (Done _ _, steps) -> steps == 4
      --   _                  -> False


-- | Helpers

-- Parses a single line with a header at a time
-- feed :: Maybe (String, [String]) -> (ByteString -> HeaderParser (V.Vector a)) -> HeaderParser (V.Vector a)
-- feed input f =
--   case input of
--     Just (firstLine, nextLine:_) -> f (pack $ unlines [firstLine, nextLine])
--     Just (_, [])                 -> DoneH mempty V.empty
--     _                            -> DoneH mempty V.empty

-- Parses the given string until the end
-- goParser :: Int -> String -> HeaderParser (V.Vector a) -> (HeaderParser (V.Vector a), Int)
-- goParser !acc input (PartialH f) =
--   goParser (acc + 1) input (feed input' f)
--     where
--       input' =
--         case uncons (lines input) of
--           Just (fl, nls) -> Just (fl, drop acc nls)
--           _              -> Nothing
-- goParser acc _ r                   = (r, acc)

type ParseError = ParseErrorBundle ByteString I.ConversionError

parseAccumulator :: ByteString -> V.Vector (Either ParseError Person)
parseAccumulator input =
  let loop _ acc (Fail bs pe) = error "unexpected error"
      loop i acc (Many rs k)  = loop (i + 1) (acc <> rs) (feed i k)
      loop _ acc (Done rs)    = acc <> rs
      feed i k =
        if i > 10
        then k mempty
        else k (take size $ drop n input)
          where
            size = 10
            n = i * size
  in
    loop 0 V.empty decode
