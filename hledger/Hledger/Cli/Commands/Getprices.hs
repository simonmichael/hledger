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
import Control.Monad (unless)
import Data.List (nub)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import System.Console.CmdArgs.Explicit
import System.Directory (findExecutable)
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

-- | How the command's stdout will be redirected, used only for logging.
-- The actual handle is set up by the caller via 'withFileOrStdout'.
data Redirect
  = ToStdout              -- ^ stdout (-o -); shown without any "> ..." suffix
  | ToFile FilePath -- ^ first write to a file; shown as " >FILE"
  | ToFileAppend FilePath   -- ^ subsequent writes to the same file; shown as " >>FILE"

redirSuffix :: Redirect -> String
redirSuffix ToStdout         = ""
redirSuffix (ToFile f)       = " >"  <> f
redirSuffix (ToFileAppend f) = " >>" <> f

-- | The getprices command.
getprices :: CliOpts -> Journal -> IO ()
getprices CliOpts{rawopts_=rawopts} j = do
  let dryRun  = boolopt "dry-run" rawopts
      moutput = case stringopt "output" rawopts of
                  "" -> Nothing
                  s  -> Just s
  mscript <- if dryRun then return (Just scriptName) else findExecutable scriptName
  script <- case mscript of
    Just s  -> return s
    Nothing -> error' $ unlines
      [scriptName <> " was not found on PATH."
      ,"Please install bin/" <> scriptName <> " from the hledger source tree,"
      ,"or your own script, in $PATH."
      ]
  today <- getCurrentDay
  let base      = journalBaseCurrencyCode j
      dir       = takeDirectory (journalFilePath j)
      -- Per-code (start, end) date span, computed in one pass over postings.
      commspans = commodityDateSpansByCode (journalPostings j)
      -- Normalise journal commodities to ISO codes / tickers, drop the base
      -- currency, deduplicate. Multiple raw symbols can map to one code
      -- (eg "$" and "USD"); we fetch once per code.
      codes   = nub . filter (/= base) . map toCurrencyCode $ journalCommoditiesUsed j
      runFor  = runFetch script base commspans today dryRun
  case moutput of
    -- Single combined output to stdout; just print each commodity's output.
    -- No file is created so empty-output handling is trivial.
    Just "-" ->
      mapM_ (\code -> runFor ToStdout code >>= maybe (return ()) putStr) codes
    -- Single combined output to a file. Capture every commodity's output
    -- first; only write the file if at least one commodity produced data.
    -- Log the redirection as " >FILE" for the first and " >>FILE" for the
    -- rest, matching what would happen if all were non-empty.
    Just outfile -> do
      let redirs = ToFile outfile : repeat (ToFileAppend outfile)
      outs <- mapM (\(r, code) -> runFor r code) (zip redirs codes)
      let combined = concat (catMaybes outs)
      unless (null combined) $ writeFile outfile combined
    -- One file per commodity, in the journal directory. Skip writing for
    -- commodities that produced no output, so we don't create empty files.
    Nothing ->
      mapM_ (\code -> do
              let outfile = dir </> "P" <> T.unpack code <> ".prices"
              mout <- runFor (ToFile outfile) code
              maybe (return ()) (writeFile outfile) mout)
            codes

-- | Fetch prices for one commodity. Logs the command line to stderr (always),
-- runs the script (unless dry-run), and forwards its stderr.
-- Returns 'Nothing' if there are no prices to write — either because the
-- script failed (in which case a warning has been logged to stderr) or
-- because it produced no output. Otherwise returns 'Just' the captured stdout.
runFetch :: FilePath -> CurrencyCode -> M.Map CurrencyCode (Day, Day) -> Day -> Bool
         -> Redirect -> CurrencyCode -> IO (Maybe String)
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
              warn (scriptName <> " failed for " <> code' <> ": " <> show e) $ return ()
              return Nothing
            Right (ExitFailure n, _, err) -> do
              hPutStr stderr err
              warn (scriptName <> " exited " <> show n <> " for " <> code') $ return ()
              return Nothing
            Right (ExitSuccess, out, err) -> do
              hPutStr stderr err
              return $ if null out then Nothing else Just out

-- | Identify the (earliest, latest) posting date for each commodity used in these postings' main amounts.
commodityDateSpansByCode :: [Posting] -> M.Map CurrencyCode (Day, Day)
commodityDateSpansByCode = foldl' addPosting M.empty
  where
    addPosting m p = foldl' (addCode (postingDate p)) m (codesIn p)
    addCode d m c  = M.insertWith merge c (d, d) m
    merge (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)
    codesIn p      = map (toCurrencyCode . acommodity) $ amountsRaw (pamount p)
