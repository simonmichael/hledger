{-|

hledger's cmdargs modes parse command-line arguments to an
intermediate format, RawOpts (an association list), rather than a
fixed ADT like CliOpts. This allows the modes and flags to be reused
more easily by hledger commands/scripts in this and other packages.

-}

module Hledger.Data.RawOptions (
  RawOpts,
  setopt,
  setboolopt,
  inRawOpts,
  boolopt,
  stringopt,
  maybestringopt,
  listofstringopt,
  intopt,
  maybeintopt,
  optserror
)
where

import Data.Maybe
import qualified Data.Text as T
import Safe

import Hledger.Utils


-- | The result of running cmdargs: an association list of option names to string values.
type RawOpts = [(String,String)]

setopt :: String -> String -> RawOpts -> RawOpts
setopt name val = (++ [(name, quoteIfNeeded $ val)])

setboolopt :: String -> RawOpts -> RawOpts
setboolopt name = (++ [(name,"")])

-- | Is the named option present ?
inRawOpts :: String -> RawOpts -> Bool
inRawOpts name = isJust . lookup name

boolopt :: String -> RawOpts -> Bool
boolopt = inRawOpts

maybestringopt :: String -> RawOpts -> Maybe String
maybestringopt name = maybe Nothing (Just . T.unpack . stripquotes . T.pack) . lookup name . reverse

stringopt :: String -> RawOpts -> String
stringopt name = fromMaybe "" . maybestringopt name

listofstringopt :: String -> RawOpts -> [String]
listofstringopt name rawopts = [v | (k,v) <- rawopts, k==name]

maybeintopt :: String -> RawOpts -> Maybe Int
maybeintopt name rawopts =
    let ms = maybestringopt name rawopts in
    case ms of Nothing -> Nothing
               Just s -> Just $ readDef (optserror $ "could not parse "++name++" number: "++s) s

intopt :: String -> RawOpts -> Int
intopt name = fromMaybe 0 . maybeintopt name

-- | Raise an error, showing the specified message plus a hint about --help.
optserror :: String -> a
optserror = error' . (++ " (run with --help for usage)")

