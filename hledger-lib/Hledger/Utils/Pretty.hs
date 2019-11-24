module Hledger.Utils.Pretty
( module Hledger.Utils.Pretty
  -- * Re-export some stuff to avoid ugly imports
, Doc
, flatAlt
, encloseSep
, annotate
, AnsiStyle
, Color(..)
, colorDull
, color
)  where

import Data.String
import Safe (maximumDef)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Terminal

class Pretty a where
    pretty :: a -> Doc AnsiStyle

-- | Align multiple documents joined vertically to the right
--
-- You may want to override flat representation.
--
-- >>> vcatRight ["abc", "defg", "hi"]
--  abc
-- defg
--   hi
vcatRight :: [Doc a] -> Doc a
vcatRight docs = vcat [indent (totalWidth - width) doc | (width, doc) <- measured]
  where
    measured = [(docWidth doc, doc) | doc <- docs]
    totalWidth = maximumDef 0 $ map fst measured

-- | Try to re-group 'Doc' horizontally.
hgroup :: Doc a -> Doc a
hgroup = group

-- | Simple estimation of 'Doc' width given that there is no width limit
--
-- >>> docWidth "abc\ndefg\nhi"
-- 4
--
-- Note that current implementation is really dummy...
docWidth :: Doc a -> Int
docWidth = maximumDef 0 . map length . lines . renderString . layoutWide where

layoutWide :: Doc a -> SimpleDocStream a
layoutWide = layoutPretty defaultLayoutOptions{layoutPageWidth = Unbounded}

-- Some compatibility between various pretty-printers

plain :: Doc a -> Doc b
plain = unAnnotate

text :: String -> Doc ann
text = fromString

-- Temporary utility for show*/cshow* functions.
-- TODO: drop once fully migrated to 'Doc'
showPretty, cshowPretty :: Pretty a => a -> String
showPretty = showWide . plain . pretty
cshowPretty = showWide . pretty

showWide :: Doc AnsiStyle -> String
showWide = T.unpack . renderStrict . layoutWide
