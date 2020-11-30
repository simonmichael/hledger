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
  appendopts,
  inRawOpts,
  boolopt,
  choiceopt,
  collectopts,
  stringopt,
  maybestringopt,
  listofstringopt,
  intopt,
  posintopt,
  maybeintopt,
  maybeposintopt,
  maybecharopt,
  overRawOpts
)
where

import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Default (Default(..))
import Safe (headMay, lastMay, readDef)

import Hledger.Utils


-- | The result of running cmdargs: an association list of option names to string values.
newtype RawOpts = RawOpts { unRawOpts :: [(String,String)] }
  deriving (Show)

instance Default RawOpts where def = RawOpts []

overRawOpts f = RawOpts . f . unRawOpts

setopt :: String -> String -> RawOpts -> RawOpts
setopt name val = overRawOpts (++ [(name, val)])

setboolopt :: String -> RawOpts -> RawOpts
setboolopt name = overRawOpts (++ [(name,"")])

appendopts :: [(String,String)] -> RawOpts -> RawOpts
appendopts new = overRawOpts $ \old -> concat [old,new]

-- | Is the named option present ?
inRawOpts :: String -> RawOpts -> Bool
inRawOpts name = isJust . lookup name . unRawOpts

boolopt :: String -> RawOpts -> Bool
boolopt = inRawOpts

-- | From a list of RawOpts, get the last one (ie the right-most on the command line)
-- for which the given predicate returns a Just value.
-- Useful for exclusive choice flags like --daily|--weekly|--quarterly...
--
-- >>> import Safe (readMay)
-- >>> choiceopt Just (RawOpts [("a",""), ("b",""), ("c","")])
-- Just "c"
-- >>> choiceopt (const Nothing) (RawOpts [("a","")])
-- Nothing
-- >>> choiceopt readMay (RawOpts [("LT",""),("EQ",""),("Neither","")]) :: Maybe Ordering
-- Just EQ
choiceopt :: (String -> Maybe a) -- ^ "parser" that returns 'Just' value for valid choice
          -> RawOpts             -- ^ actual options where to look for flag
          -> Maybe a             -- ^ exclusive choice among those returned as 'Just' from "parser"
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

-- | Reads the named option's Int argument, if it is present.
-- An argument that is too small or too large will raise an error.
maybeintopt :: String -> RawOpts -> Maybe Int
maybeintopt = maybeclippedintopt minBound maxBound

-- | Reads the named option's natural-number argument, if it is present.
-- An argument that is negative or too large will raise an error.
maybeposintopt :: String -> RawOpts -> Maybe Int
maybeposintopt = maybeclippedintopt 0 maxBound

-- | Reads the named option's Int argument. If not present it will
-- return 0. An argument that is too small or too large will raise an error.
intopt :: String -> RawOpts -> Int
intopt name = fromMaybe 0 . maybeintopt name

-- | Reads the named option's natural-number argument. If not present it will
-- return 0. An argument that is negative or too large will raise an error.
posintopt :: String -> RawOpts -> Int
posintopt name = fromMaybe 0 . maybeposintopt name

-- | Reads the named option's Int argument, if it is present. An argument
-- that does not fit within the given bounds will raise an error.
maybeclippedintopt :: Int -> Int -> String -> RawOpts -> Maybe Int
maybeclippedintopt minVal maxVal name =
    fmap (intOrError . readOrError) . maybestringopt name
  where
    readOrError s = readDef (usageError $ "could not parse " ++ name ++ " number: " ++ s) s
    intOrError n | n >= toInteger minVal && n <= toInteger maxVal = fromInteger n
                 | otherwise = usageError $ "argument to " ++ name
                                         ++ " must lie in the range "
                                         ++ show minVal ++ " to " ++ show maxVal
                                         ++ ", but is " ++ show n
