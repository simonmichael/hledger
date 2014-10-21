-- | Web handler utilities. More of these are in Foundation.hs, where
-- they can be used in the default template.

module Handler.Utils where

import Prelude
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)


numbered :: [a] -> [(Int,a)]
numbered = zip [1..]

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d = read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
                     where t = UTCTime d (secondsToDiffTime 0)
