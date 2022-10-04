module Data.Csv.Parser.Megaparsec.Incremental (decodeHeader) where

import qualified Data.ByteString                      as B
import           Data.ByteString.Lazy                 (fromStrict)
import           Data.Csv                             (FromNamedRecord,
                                                       defaultDecodeOptions)
import           Data.Csv.Incremental                 hiding (decodeHeader)
import           Data.Csv.Parser.Megaparsec.Internals (csvWithHeader)
import qualified Data.Vector                          as V
import           Text.Megaparsec                      (errorBundlePretty, parse)


-- | TODO: Document params and function
decodeHeader :: FromNamedRecord a => HeaderParser (V.Vector a)
decodeHeader = PartialH (go . parser)
  where
    parser bs =
      -- TODO: What to return if input is empty (e.g. when we reach the eof)?
      if B.null bs
        then Right (mempty, mempty)
        else parse (csvWithHeader defaultDecodeOptions) "" (fromStrict bs)

    -- | Collects results in a vector
    go = go' V.empty
    go' _ (Left pe) = FailH mempty (errorBundlePretty pe)
    go' acc (Right (h, r)) =
      if V.null r
      then DoneH h acc
      else PartialH $ \rbs -> go' (r <> acc) (parser rbs)
