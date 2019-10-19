{-# LANGUAGE DeriveDataTypeable #-}

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
  choiceopt,
  collectopts,
  stringopt,
  maybestringopt,
  listofstringopt,
  intopt,
  maybeintopt,
  maybecharopt
)
where

import Data.Maybe
import Data.Data
import Data.Default
import Safe

import Hledger.Utils


-- | The result of running cmdargs: an association list of option names to string values.
newtype RawOpts = RawOpts { unRawOpts :: [(String,String)] }
    deriving (Show, Data, Typeable)

instance Default RawOpts where def = RawOpts []

overRawOpts f = RawOpts . f . unRawOpts

setopt :: String -> String -> RawOpts -> RawOpts
setopt name val = overRawOpts (++ [(name, val)])

setboolopt :: String -> RawOpts -> RawOpts
setboolopt name = overRawOpts (++ [(name,"")])

-- | Is the named option present ?
inRawOpts :: String -> RawOpts -> Bool
inRawOpts name = isJust . lookup name . unRawOpts

boolopt :: String -> RawOpts -> Bool
boolopt = inRawOpts

-- | Get latests successfully parsed flag
--
-- >>> choiceopt Just (RawOpts [("a",""), ("b",""), ("c","")])
-- Just "c"
-- >>> choiceopt (const Nothing) (RawOpts [("a","")])
-- Nothing
-- >>> choiceopt (listToMaybe . filter (`elem` ["a","b"])) (RawOpts [("a",""), ("b",""), ("c","")])
-- Just "b"
choiceopt :: (String -> Maybe a) -> RawOpts -> Maybe a
choiceopt f = lastMay . collectopts (f . fst)

-- | Collects processed and filtered list of options preserving their order
--
-- >>> collectopts (const Nothing) (RawOpts [("x","")])
-- []
-- >>> collectopts Just (RawOpts [("a",""),("b","")])
-- [("a",""),("b","")]
collectopts :: ((String, String) -> Maybe a) -> RawOpts -> [a]
collectopts f = mapMaybe f . unRawOpts

maybestringopt :: String -> RawOpts -> Maybe String
maybestringopt name = lookup name . reverse . unRawOpts

stringopt :: String -> RawOpts -> String
stringopt name = fromMaybe "" . maybestringopt name

maybecharopt :: String -> RawOpts -> Maybe Char
maybecharopt name (RawOpts rawopts) = lookup name rawopts >>= headMay

listofstringopt :: String -> RawOpts -> [String]
listofstringopt name (RawOpts rawopts) = [v | (k,v) <- rawopts, k==name]

maybeintopt :: String -> RawOpts -> Maybe Int
maybeintopt name rawopts =
    let ms = maybestringopt name rawopts in
    case ms of Nothing -> Nothing
               Just s -> Just $ readDef (usageError $ "could not parse "++name++" number: "++s) s

intopt :: String -> RawOpts -> Int
intopt name = fromMaybe 0 . maybeintopt name

