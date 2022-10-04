{-# LANGUAGE BangPatterns #-}

module Data.Csv.Parser.Megaparsec.Incremental (decodeHeader) where

import           Data.ByteString                      (ByteString)
import           Data.ByteString.Lazy                 (fromStrict)
import           Data.Csv                             (FromNamedRecord,
                                                       defaultDecodeOptions)
import           Data.Csv.Incremental                 hiding (decodeHeader)
import           Data.Csv.Parser.Megaparsec.Internals (csvWithHeader)
import qualified Data.Vector                          as V
import           Text.Megaparsec                      (errorBundlePretty, parse)


-- | TODO: Add something here
decodeHeader :: FromNamedRecord a => HeaderParser (V.Vector a)
decodeHeader = PartialH (\bs ->
    case parser (fromStrict bs) of
      (Left parseError) -> FailH mempty (errorBundlePretty parseError)
      (Right (h, r))    -> DoneH h r
  )
  where
    parser = parse (csvWithHeader defaultDecodeOptions) ""
