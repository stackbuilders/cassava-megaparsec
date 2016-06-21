-- |
-- Module      :  Data.Csv.Parser.Megaparsec
-- Copyright   :  © 2016 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A CSV parser. The parser here is RFC 4180 compliant, with the following
-- extensions:
--
--     * Empty lines are ignored.
--     * Non-escaped fields may contain any characters except double-quotes,
--       commas, carriage returns, and newlines.
--     * Escaped fields may contain any characters (but double-quotes need
--       to be escaped).
--
-- The parser provides better error messages than the parser that comes with
-- Cassava library, while being compatible with the rest of the library.

module Data.Csv.Parser.Megaparsec
  (  )
where
