module Data.Csv.Parser.Megaparsec.Incremental (Parser(..), decode) where

import qualified Debug.Trace                          as DT

import           Data.ByteString.Lazy                 as BL

import           Data.Csv                             hiding (Parser, decode,
                                                       record, runParser)
import qualified Data.Csv                             as C
import qualified Data.Csv.Parser.Megaparsec.Internals as I

import qualified Data.Vector                          as V

import           Text.Megaparsec                      (ParseErrorBundle, parse)


type ParseError = ParseErrorBundle BL.ByteString I.ConversionError

data Parser a
  = Fail !BL.ByteString ParseError
  | Many (V.Vector (Either ParseError a)) (BL.ByteString -> Parser a)
  | Done (V.Vector (Either ParseError a))

-- | Parses incrementally using 'csvWithHeader' and 'defaultDecodeOptions'.
--
-- Note: Given an empty 'ByteString' will make the parser fail. Therefore, make
-- sure you handle that one level above (before passing that to this parser).
decode
  :: C.FromRecord a
  => Parser a
decode = Many V.empty (go V.empty)
  where
    runParser :: C.FromRecord a => BL.ByteString -> Either ParseError a
    runParser =
      parse (I.record (C.decDelimiter C.defaultDecodeOptions) C.parseRecord) ""
      -- parse (I.record (C.decDelimiter C.defaultDecodeOptions) (_ C.parseRecord)) ""

    go :: C.FromRecord a => V.Vector (Either ParseError a) -> ByteString -> Parser a
    go acc bs =
      if BL.null bs
      -- if BL.null (DT.traceShowId bs)

      then Done acc
      else
        case runParser input of
          Left e  ->
            Many (Left e `V.cons` acc) (go V.empty . (rest <>))
          Right v ->
            Many (Right v `V.cons` acc) (go V.empty . (rest <>))
      where
        -- TODO: `span` and `break` put '\n' on the left but we need to get rid
        -- of it otherwise we end up accumulating the rest.
        eol = 10
        (input, rest) = DT.traceShowId $ BL.span (/= eol) bs


  --   go' bs _ (Left pe) =
  --     FailH bs (errorBundlePretty pe)

  --   go' _ acc (Right (h, r)) =
  --     if V.null r
  --     then DoneH h acc
  --     else PartialH $ \rbs -> go' rbs (r <> acc) (parser rbs)
