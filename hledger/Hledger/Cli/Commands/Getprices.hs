{-|

The @getprices@ command fetches market prices for the commodities used in
the journal, by calling the bin/getprices_ shell script (which dispatches
to pricehist or similar tools) once per commodity. Output is saved to
P<COMM>.prices files in the same directory as the main journal file.

-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Cli.Commands.Getprices (
  getpricesmode
 ,getprices
) where

import Control.Exception (IOException, try)
import Control.Monad (unless, (>=>))
import Data.List (nub, sortOn)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Calendar (Day)
import System.Console.CmdArgs.Explicit
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (proc, readCreateProcessWithExitCode)

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
getpricesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Getprices.txt")
  [flagNone ["dry-run"] (setboolopt "dry-run") "just print the commands that would be run"
  ,flagReq  ["output","o"] (\s opts -> Right $ setopt "output" s opts) "FILE"
     "write all prices to FILE (or - for stdout) instead of per-commodity P<COMM>.prices files"
  ]
  cligeneralflagsgroups2
  hiddenflags
  ([], Nothing)

scriptName :: FilePath
scriptName = "getprices_"

-- | The path of the per-commodity prices file in 'dir' for the given code.
pricesFileFor :: FilePath -> CurrencyCode -> FilePath
pricesFileFor dir code = dir </> "prices" </> T.unpack code <> ".prices"

-- | Like 'warn' but throws away the trailing-action argument; for the common
-- "log a warning, then continue" pattern.
warn_ :: String -> IO ()
warn_ msg = warn msg (return ())

-- | Parse a journal-format text blob and extract its P directives.
-- Uses the 'ExceptT'-returning 'readJournal' so parse errors come back
-- as 'Left', without exception-throwing.
parsePrices :: T.Text -> IO (Either String [PriceDirective])
parsePrices txt = do
  hdl <- textToHandle txt
  fmap jpricedirectives <$> runExceptT (readJournal definputopts Nothing hdl)

-- | How the command's stdout will be presented, used only for logging.
-- The actual file write is done by the merge code at the end of the run.
data Redirect
  = ToStdout         -- ^ stdout (-o -); shown without any "> ..." suffix
  | ToFile FilePath  -- ^ collected into a file; shown as " >FILE"

redirSuffix :: Redirect -> String
redirSuffix ToStdout    = ""
redirSuffix (ToFile f)  = " >" <> f

-- | The getprices command.
getprices :: CliOpts -> Journal -> IO ()
getprices CliOpts{rawopts_=rawopts} j = do
  let
    dryRun  = boolopt "dry-run" rawopts
    moutput = case stringopt "output" rawopts of
                "" -> Nothing
                s  -> Just s
    dir         = takeDirectory (journalFilePath j)
    localScript = dir </> "prices" </> scriptName
    notFound = error' $ unlines
      [scriptName <> " was not found in JOURNALDIR/prices/ or in PATH."
      ,"Please install bin/" <> scriptName <> " from the hledger source tree,"
      ,"or your own script, in your prices/ directory or in $PATH."
      ]
  script <- if dryRun
              then return scriptName
              else do
                localExists <- doesFileExist localScript
                if localExists
                  then return localScript
                  else findExecutable scriptName >>= maybe notFound return
  today <- getCurrentDay
  let
    base      = journalBaseCurrencyCode j
    -- Per-code (start, end) date span, computed in one pass over postings.
    commspans = commodityDateSpansByCode (journalPostings j)
    -- Normalise journal commodities to ISO codes / tickers, drop the base
    -- currency, deduplicate. Multiple raw symbols can map to one code
    -- (eg "$" and "USD"); we fetch once per code.
    codes   = nub . filter (/= base) . map toCurrencyCode $ journalCommoditiesUsed j
    runFor  = runFetch script base commspans today dryRun
  case moutput of
    -- Combined output to stdout; just print each commodity's output.
    -- No file is read or written, so the merge logic doesn't apply.
    Just "-" ->
      mapM_ (\code -> runFor ToStdout code >>= maybe (return ()) T.putStr) codes
    -- Combined output to a single file. Collect each commodity's output,
    -- then merge once with whatever's already at outfile.
    Just outfile -> do
      outs <- mapM (runFor (ToFile outfile)) codes
      let combined = T.concat (catMaybes outs)
      unless (T.null combined) $
        mergeAndWritePrices outfile combined >>= reportOutcome outfile
    -- One file per commodity, merged with that file's existing prices.
    Nothing ->
      mapM_ (\code -> do
              let outfile = pricesFileFor dir code
              mout <- runFor (ToFile outfile) code
              maybe (return ()) (mergeAndWritePrices outfile >=> reportOutcome outfile) mout)
            codes

-- | Fetch prices for one commodity. Logs the command line to stderr (always),
-- runs the script (unless dry-run), and forwards its stderr.
-- Returns 'Nothing' if there are no prices to write — either because the
-- script failed (in which case a warning has been logged to stderr) or
-- because it produced no output. Otherwise returns 'Just' the captured stdout.
runFetch :: FilePath -> CurrencyCode -> M.Map CurrencyCode (Day, Day) -> Day -> Bool
         -> Redirect -> CurrencyCode -> IO (Maybe T.Text)
runFetch script base commspans today dryRun redir code =
  case M.lookup code commspans of
    Nothing -> return Nothing
    Just (start, _end) -> do
      let code'   = T.unpack code
          args    = [T.unpack base, code', show start, show today]
          cmdline = unwords (scriptName : args) <> redirSuffix redir
      hPutStrLn stderr cmdline
      if dryRun
        then return Nothing
        else do
          eres <- try (readCreateProcessWithExitCode (proc script args) "")
                    :: IO (Either IOException (ExitCode, String, String))
          case eres of
            Left e -> do
              warn_ $ scriptName <> " failed for " <> code' <> ": " <> show e
              return Nothing
            Right (ec, out, err) -> do
              hPutStr stderr err
              case ec of
                ExitFailure n -> do
                  warn_ $ scriptName <> " exited " <> show n <> " for " <> code'
                  return Nothing
                ExitSuccess ->
                  return $ if null out then Nothing else Just (T.pack out)

-- | Key by which we deduplicate two P directives.
priceKey :: PriceDirective -> (Day, CommoditySymbol, CommoditySymbol)
priceKey pd = (pddate pd, pdcommodity pd, acommodity (pdamount pd))

-- | Outcome of attempting to merge new prices into a file.
data MergeOutcome
  = MergedWritten Int Int       -- ^ wrote the file: existing kept, new added
  | NothingNew                  -- ^ no new keys; file untouched
  | NotPureFileRefused          -- ^ existing file has non-price content; refused
  | PriceParseError String      -- ^ couldn't parse a prices blob

-- | Quick textual safeguard: file contains only P directives and blank lines.
-- Comments, transactions, account or other directives all disqualify the
-- file because re-rendering through hledger would lose them.
isPurePricesFile :: T.Text -> Bool
isPurePricesFile =
  all (\line -> let l = T.strip line in T.null l || "P" `T.isPrefixOf` l)
  . T.lines

-- | Load existing P directives from a file. Empty list if file is missing.
-- Returns Left if the file exists but contains content other than P
-- directives / blank lines, or if it can't be parsed.
loadExistingPrices :: FilePath -> IO (Either MergeOutcome [PriceDirective])
loadExistingPrices f = do
  exists <- doesFileExist f
  if not exists
    then return (Right [])
    else do
      txt <- T.readFile f
      if not (isPurePricesFile txt)
        then return (Left NotPureFileRefused)
        else either (Left . PriceParseError) Right <$> parsePrices txt

-- | Merge new prices into a destination file (creating if needed).
-- 'newcontent' is the raw text we'd otherwise write — a journal-format
-- blob produced by getprices_. Existing prices win on key conflicts;
-- new prices on unseen keys are added. The merged set is written sorted
-- ascending by (date, from, to). If nothing new would be added, the file
-- is left untouched.
mergeAndWritePrices :: FilePath -> T.Text -> IO MergeOutcome
mergeAndWritePrices outfile newcontent = do
  eexisting <- loadExistingPrices outfile
  case eexisting of
    Left outcome -> return outcome
    Right existing -> do
      enew <- parsePrices newcontent
      case enew of
        Left e -> return (PriceParseError ("could not parse new prices: " <> e))
        Right newpds -> do
          let existingKeys = S.fromList (map priceKey existing)
              added        = filter ((`S.notMember` existingKeys) . priceKey) newpds
          if null added
            then return NothingNew
            else do
              let merged = sortOn priceKey (existing <> added)
                  out    = T.unlines (map showPriceDirective merged)
              createDirectoryIfMissing True (takeDirectory outfile)
              T.writeFile outfile out
              return (MergedWritten (length existing) (length added))

-- | Write a one-line summary of a merge outcome to stderr.
reportOutcome :: FilePath -> MergeOutcome -> IO ()
reportOutcome outfile outcome = hPutStrLn stderr $ case outcome of
  MergedWritten ex ad ->
    "wrote " <> outfile <> " (existing " <> show ex <> ", added " <> show ad <> ")"
  NothingNew ->
    outfile <> ": no new prices"
  NotPureFileRefused ->
    "Refusing to overwrite " <> outfile <>
    ": it contains content other than P directives, which would be lost."
  PriceParseError msg ->
    outfile <> ": could not read existing prices: " <> msg

-- | Identify the (earliest, latest) posting date for each commodity used in these postings' main amounts.
commodityDateSpansByCode :: [Posting] -> M.Map CurrencyCode (Day, Day)
commodityDateSpansByCode = foldl' addPosting M.empty
  where
    addPosting m p = foldl' (addCode (postingDate p)) m (codesIn p)
    addCode d m c  = M.insertWith merge c (d, d) m
    merge (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)
    codesIn p      = map (toCurrencyCode . acommodity) $ amountsRaw (pamount p)
