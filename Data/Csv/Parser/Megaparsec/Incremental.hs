module Data.Csv.Parser.Megaparsec.Incremental (decodeHeader) where

import qualified Data.ByteString                      as B
import           Data.ByteString.Lazy                 (fromStrict)
import           Data.Csv                             (FromNamedRecord,
                                                       defaultDecodeOptions)
import           Data.Csv.Incremental                 hiding (decodeHeader)
import           Data.Csv.Parser.Megaparsec.Internals (csvWithHeader)
import qualified Data.Vector                          as V
import           Text.Megaparsec                      (errorBundlePretty, parse)


-- | Parses incrementally using 'csvWithHeader' and 'defaultDecodeOptions'.
--
-- Note: Given an empty 'ByteString' will make the parser fail. Therefore, make
-- sure you handle that one level above (before passing that to this parser).
decodeHeader
  :: FromNamedRecord a
  => HeaderParser (V.Vector a)
decodeHeader = PartialH go
  where
    parser =
      parse (csvWithHeader defaultDecodeOptions) "" . fromStrict

    -- | Collects header and results
    go bs = go' bs mempty (parser bs)

    go' bs _ (Left pe) =
      FailH bs (errorBundlePretty pe)

    go' _ acc (Right (h, r)) =
      if V.null r
      then DoneH h acc
      else PartialH $ \rbs -> go' rbs (r <> acc) (parser rbs)
