-- | Calculate the width of String and Text, being aware of wide characters.

module Text.WideString (
  -- * wide-character-aware layout
  strWidth,
  textWidth,
  charWidth,
  -- * Text Builders which keep track of length
  WideBuilder(..),
  wbUnpack,
  wbToText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB


-- | Helper for constructing Builders while keeping track of text width.
data WideBuilder = WideBuilder
  { wbBuilder :: !TB.Builder
  , wbWidth   :: !Int
  } deriving (Show)

instance Semigroup WideBuilder where
  WideBuilder x i <> WideBuilder y j = WideBuilder (x <> y) (i + j)

instance Monoid WideBuilder where
  mempty = WideBuilder mempty 0

-- | Convert a WideBuilder to a strict Text.
wbToText :: WideBuilder -> Text
wbToText = TL.toStrict . TB.toLazyText . wbBuilder

-- | Convert a WideBuilder to a String.
wbUnpack :: WideBuilder -> String
wbUnpack = TL.unpack . TB.toLazyText . wbBuilder


-- | Calculate the render width of a string, considering
-- wide characters (counted as double width)
strWidth :: String -> Int
strWidth = foldr (\a b -> charWidth a + b) 0

-- | Calculate the render width of a string, considering
-- wide characters (counted as double width)
textWidth :: Text -> Int
textWidth = T.foldr (\a b -> charWidth a + b) 0

-- from Pandoc (copyright John MacFarlane, GPL)
-- see also http://unicode.org/reports/tr11/#Description

-- | Get the designated render width of a character: 0 for a combining
-- character, 1 for a regular character, 2 for a wide character.
-- (Wide characters are rendered as exactly double width in apps and
-- fonts that support it.) (From Pandoc.)
charWidth :: Char -> Int
charWidth c
    | c <  '\x0300'                    = 1
    | c >= '\x0300' && c <= '\x036F'   = 0  -- combining
    | c >= '\x0370' && c <= '\x10FC'   = 1
    | c >= '\x1100' && c <= '\x115F'   = 2
    | c >= '\x1160' && c <= '\x11A2'   = 1
    | c >= '\x11A3' && c <= '\x11A7'   = 2
    | c >= '\x11A8' && c <= '\x11F9'   = 1
    | c >= '\x11FA' && c <= '\x11FF'   = 2
    | c >= '\x1200' && c <= '\x2328'   = 1
    | c >= '\x2329' && c <= '\x232A'   = 2
    | c >= '\x232B' && c <= '\x2E31'   = 1
    | c >= '\x2E80' && c <= '\x303E'   = 2
    | c == '\x303F'                    = 1
    | c >= '\x3041' && c <= '\x3247'   = 2
    | c >= '\x3248' && c <= '\x324F'   = 1 -- ambiguous
    | c >= '\x3250' && c <= '\x4DBF'   = 2
    | c >= '\x4DC0' && c <= '\x4DFF'   = 1
    | c >= '\x4E00' && c <= '\xA4C6'   = 2
    | c >= '\xA4D0' && c <= '\xA95F'   = 1
    | c >= '\xA960' && c <= '\xA97C'   = 2
    | c >= '\xA980' && c <= '\xABF9'   = 1
    | c >= '\xAC00' && c <= '\xD7FB'   = 2
    | c >= '\xD800' && c <= '\xDFFF'   = 1
    | c >= '\xE000' && c <= '\xF8FF'   = 1 -- ambiguous
    | c >= '\xF900' && c <= '\xFAFF'   = 2
    | c >= '\xFB00' && c <= '\xFDFD'   = 1
    | c >= '\xFE00' && c <= '\xFE0F'   = 1 -- ambiguous
    | c >= '\xFE10' && c <= '\xFE19'   = 2
    | c >= '\xFE20' && c <= '\xFE26'   = 1
    | c >= '\xFE30' && c <= '\xFE6B'   = 2
    | c >= '\xFE70' && c <= '\xFEFF'   = 1
    | c >= '\xFF01' && c <= '\xFF60'   = 2
    | c >= '\xFF61' && c <= '\x16A38'  = 1
    | c >= '\x1B000' && c <= '\x1B001' = 2
    | c >= '\x1D000' && c <= '\x1F1FF' = 1
    | c >= '\x1F200' && c <= '\x1F251' = 2
    | c >= '\x1F300' && c <= '\x1F773' = 1
    | c >= '\x20000' && c <= '\x3FFFD' = 2
    | otherwise                        = 1
