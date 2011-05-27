{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-

hledger-web's request handlers, and helpers.

-}

module Handlers where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Either (lefts,rights)
import Data.List
import Data.Maybe
import Data.Text(Text,pack,unpack)
import Data.Time.Calendar
import System.FilePath (takeFileName, (</>))
import System.IO.Storage (putValue, getValue)
import Text.Hamlet hiding (hamletFile)
import Text.ParserCombinators.Parsec -- hiding (string)
import Text.Printf
import Text.RegexPR
import Yesod.Form
import Yesod.Json

import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Cli.Version (version)
import Hledger.Cli.Version -- XXX
import Hledger.Data hiding (today)
import Hledger.Read (journalFromPathAndString)
import Hledger.Read.JournalReader (someamount)
import Hledger.Utils

import App
import Settings


getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" $ Settings.staticdir </> "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

getRootR :: Handler RepHtml
getRootR = redirect RedirectTemporary defaultroute where defaultroute = RegisterR

----------------------------------------------------------------------
-- main views

-- | The main journal view, with accounts sidebar.
getJournalR :: Handler RepHtml
getJournalR = do
  vd@VD{opts=opts,fspec=fspec,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml opts vd $ balanceReport opts fspec j
      maincontent = journalReportAsHtml opts vd $ journalReport opts fspec j
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet $(Settings.hamletFile "journal")

postJournalR :: Handler RepPlain
postJournalR = handlePost

-- | The main register view, with accounts sidebar.
getRegisterR :: Handler RepHtml
getRegisterR = do
  vd@VD{opts=opts,fspec=fspec,m=m,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml opts vd $ balanceReport opts fspec $ filterJournalPostings2 m j
      maincontent = registerReportAsHtml opts vd $ registerReport opts fspec $ filterJournalPostings2 m j
      editform' = editform vd
  defaultLayout $ do
      setTitle "hledger-web register"
      addHamlet $(Settings.hamletFile "register")

postRegisterR :: Handler RepPlain
postRegisterR = handlePost

-- | A simple journal view, like hledger print (with editing.)
getJournalOnlyR :: Handler RepHtml
getJournalOnlyR = do
  vd@VD{opts=opts,fspec=fspec,j=j} <- getViewData
  defaultLayout $ do
      setTitle "hledger-web journal only"
      addHamlet $ journalReportAsHtml opts vd $ journalReport opts fspec j

postJournalOnlyR :: Handler RepPlain
postJournalOnlyR = handlePost

-- | A simple postings view, like hledger register (with editing.)
getRegisterOnlyR :: Handler RepHtml
getRegisterOnlyR = do
  vd@VD{opts=opts,fspec=fspec,j=j} <- getViewData
  defaultLayout $ do
      setTitle "hledger-web register only"
      addHamlet $ registerReportAsHtml opts vd $ registerReport opts fspec j

postRegisterOnlyR :: Handler RepPlain
postRegisterOnlyR = handlePost

-- | A simple accounts view, like hledger balance. If the Accept header
-- specifies json, returns the chart of accounts as json.
getAccountsOnlyR :: Handler RepHtmlJson
getAccountsOnlyR = do
  vd@VD{opts=opts,fspec=fspec,j=j} <- getViewData
  let json = jsonMap [("accounts", toJSON $ journalAccountNames j)]
      html = do
        setTitle "hledger-web accounts"
        addHamlet $ balanceReportAsHtml opts vd $ balanceReport opts fspec j
  defaultLayoutJson html json

-- | Return the chart of accounts as json, without needing a special Accept header.
getAccountsJsonR :: Handler RepJson
getAccountsJsonR = do
  VD{j=j} <- getViewData
  jsonToRepJson $ jsonMap [("accounts", toJSON $ journalAccountNames j)]

-- helpers

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> ViewData -> BalanceReport -> Hamlet AppRoute
balanceReportAsHtml _ vd@VD{here=here,a=a,p=p} (items,total) = $(Settings.hamletFile "balancereport")
 where
   itemAsHtml :: ViewData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml VD{p=p} (acct, adisplay, adepth, abal) = $(Settings.hamletFile "balancereportitem")
     where
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       acctpat = accountNameToAccountRegex acct
       pparam = if null p then "" else "&p="++p
   accountsheading = $(Settings.hamletFile "accountsheading")
       where
         filteringaccts = not $ null a
         showlinks = $(Settings.hamletFile "accountsheadinglinks")
         showmore = case (filteringaccts, items) of
                      -- cunning parent account logic
                      (True, ((acct, _, _, _):_)) ->
                          let a' = if isAccountRegex a then a else acct
                              a'' = accountNameToAccountRegex $ parentAccountName $ accountRegexToAccountName a'
                              parenturl = (here, [("a",pack a''), ("p",pack p)])
                          in $(Settings.hamletFile "accountsheadinglinksmore")
                      _ -> nulltemplate
         showall = if filteringaccts
                    then $(Settings.hamletFile "accountsheadinglinksall")
                    else nulltemplate
             where allurl = (here, [("p",pack p)])

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> ViewData -> JournalReport -> Hamlet AppRoute
journalReportAsHtml _ vd items = $(Settings.hamletFile "journalreport")
 where
   itemAsHtml :: ViewData -> (Int, JournalReportItem) -> Hamlet AppRoute
   itemAsHtml _ (n, t) = $(Settings.hamletFile "journalreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

-- | Render a register report as HTML.
registerReportAsHtml :: [Opt] -> ViewData -> RegisterReport -> Hamlet AppRoute
registerReportAsHtml _ vd items = $(Settings.hamletFile "registerreport")
 where
   itemAsHtml :: ViewData -> (Int, RegisterReportItem) -> Hamlet AppRoute
   itemAsHtml VD{here=here,p=p} (n, (ds, posting, b)) = $(Settings.hamletFile "registerreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       (firstposting, date, desc) = case ds of Just (da, de) -> ("firstposting", show da, de)
                                               Nothing -> ("", "", "") :: (String,String,String)
       acct = paccount posting
       acctpat = accountNameToAccountRegex acct
       pparam = if null p then "" else "&p="++p

mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ show b
    where addclass = printf "<span class=\"%s\">%s</span>" (c :: String)
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

-- | Handle a post from any of the edit forms.
handlePost :: Handler RepPlain
handlePost = do
  action <- runFormPost' $ maybeStringInput "action"
  case action of Just "add"    -> handleAdd
                 Just "edit"   -> handleEdit
                 Just "import" -> handleImport
                 _             -> invalidArgs [pack "invalid action"]

-- | Handle a post from the transaction add form.
handleAdd :: Handler RepPlain
handleAdd = do
  VD{j=j,today=today} <- getViewData
  -- get form input values. M means a Maybe value.
  (dateM, descM, acct1M, amt1M, acct2M, amt2M, journalM) <- runFormPost'
    $ (,,,,,,)
    <$> maybeStringInput "date"
    <*> maybeStringInput "description"
    <*> maybeStringInput "account1"
    <*> maybeStringInput "amount1"
    <*> maybeStringInput "account2"
    <*> maybeStringInput "amount2"
    <*> maybeStringInput "journal"
  -- supply defaults and parse date and amounts, or get errors.
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today . unpack) dateM
      descE = Right $ maybe "" unpack descM
      acct1E = maybe (Left "to account required") (Right . unpack) acct1M
      acct2E = maybe (Left "from account required") (Right . unpack) acct2M
      amt1E = maybe (Left "amount required") (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx someamount . unpack) amt1M
      amt2E = maybe (Right missingamt)       (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx someamount . unpack) amt2M
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left $ "unrecognised journal file path: " ++ f'
                              )
                       journalM
      strEs = [dateE, descE, acct1E, acct2E, journalE]
      amtEs = [amt1E, amt2E]
      errs = lefts strEs ++ lefts amtEs
      [date,desc,acct1,acct2,journalpath] = rights strEs
      [amt1,amt2] = rights amtEs
      -- if no errors so far, generate a transaction and balance it or get the error.
      tE | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (head $ lines e)]) Right
                        (balanceTransaction Nothing $ nulltransaction { -- imprecise balancing
                           tdate=parsedate date
                          ,tdescription=desc
                          ,tpostings=[
                            Posting False acct1 amt1 "" RegularPosting [] Nothing
                           ,Posting False acct2 amt2 "" RegularPosting [] Nothing
                           ]
                          })
  -- display errors or add transaction
  case tE of
   Left errs -> do
    -- save current form values in session
    setMessage $ toHtml $ intercalate "; " errs
    redirect RedirectTemporary RegisterR

   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    liftIO $ appendToJournalFile journalpath $ showTransaction t'
    setMessage $ toHtml $ (printf "Added transaction:\n%s" (show t') :: String)
    redirect RedirectTemporary RegisterR

-- | Handle a post from the journal edit form.
handleEdit :: Handler RepPlain
handleEdit = do
  VD{j=j} <- getViewData
  -- get form input values, or validation errors.
  -- getRequest >>= liftIO (reqRequestBody req) >>= mtrace
  (textM, journalM) <- runFormPost'
    $ (,)
    <$> maybeStringInput "text"
    <*> maybeStringInput "journal"
  let textE = maybe (Left "No value provided") (Right . unpack) textM
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left "unrecognised journal file path")
                       journalM
      strEs = [textE, journalE]
      errs = lefts strEs
      [text,journalpath] = rights strEs
  -- display errors or perform edit
  if not $ null errs
   then do
    setMessage $ toHtml (intercalate "; " errs :: String)
    redirect RedirectTemporary JournalR

   else do
    -- try to avoid unnecessary backups or saving invalid data
    filechanged' <- liftIO $ journalSpecifiedFileIsNewer j journalpath
    told <- liftIO $ readFileStrictly journalpath
    let tnew = filter (/= '\r') text
        changed = tnew /= told || filechanged'
    if not changed
     then do
       setMessage "No change"
       redirect RedirectTemporary JournalR
     else do
      jE <- liftIO $ journalFromPathAndString Nothing journalpath tnew
      either
       (\e -> do
          setMessage $ toHtml e
          redirect RedirectTemporary JournalR)
       (const $ do
          liftIO $ writeFileWithBackup journalpath tnew
          setMessage $ toHtml (printf "Saved journal %s\n" (show journalpath) :: String)
          redirect RedirectTemporary JournalR)
       jE

-- | Handle post from the journal import form.
handleImport :: Handler RepPlain
handleImport = do
  setMessage "can't handle file upload yet"
  redirect RedirectTemporary JournalR
  -- -- get form input values, or basic validation errors. E means an Either value.
  -- fileM <- runFormPost' $ maybeFileInput "file"
  -- let fileE = maybe (Left "No file provided") Right fileM
  -- -- display errors or import transactions
  -- case fileE of
  --  Left errs -> do
  --   setMessage errs
  --   redirect RedirectTemporary JournalR

  --  Right s -> do
  --    setMessage s
  --    redirect RedirectTemporary JournalR

----------------------------------------------------------------------
-- | Other view components.

-- | Global toolbar/heading area.
topbar :: ViewData -> Hamlet AppRoute
topbar VD{p=p,j=j,msg=msg,today=today} = $(Settings.hamletFile "topbar")
  where
    (title, desc) = journalTitleDesc j p today

-- | Generate a title and description for the given journal, period
-- expression, and date.
journalTitleDesc :: Journal -> String -> Day -> (String, String)
journalTitleDesc j p today = (title, desc)
  where
    title = printf "%s" (takeFileName $ journalFilePath j) :: String
    desc  = printf "%s" (showspan span) :: String
    span = either (const $ DateSpan Nothing Nothing) snd (parsePeriodExpr today p)
    showspan (DateSpan Nothing Nothing) = ""
    showspan s = " (" ++ dateSpanAsText s ++ ")"

-- | Links to navigate between the main views.
navlinks :: ViewData -> Hamlet AppRoute
navlinks vd = $(Settings.hamletFile "navlinks")
 where
   accountsjournallink  = navlink vd "transactions" JournalR
   accountsregisterlink = navlink vd "postings" RegisterR
   navlink :: ViewData -> String -> AppRoute -> Hamlet AppRoute
   navlink VD{here=here,a=a,p=p} s dest = $(Settings.hamletFile "navlink")
    where u = (dest, concat [(if null a then [] else [("a", pack a)])
                            ,(if null p then [] else [("p", pack p)])])
          style | dest == here = "navlinkcurrent"
                | otherwise    = "navlink" :: Text

-- | Links to the various journal editing forms.
editlinks :: Hamlet AppRoute
editlinks = $(Settings.hamletFile "editlinks")

-- | Link to a topic in the manual.
helplink :: String -> String -> Hamlet AppRoute
helplink topic label = $(Settings.hamletFile "helplink")
    where u = manualurl ++ if null topic then "" else '#':topic

-- | Form controlling journal filtering parameters.
filterform :: ViewData -> Hamlet AppRoute
filterform VD{here=here,a=a,p=p,q=q} = $(Settings.hamletFile "filterform")
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  filtering = not $ null q
  visible = "block" :: String
  filteringclass = if filtering then "filtering" else "" :: String
  filteringperiodclass = "" :: String
  stopfiltering = if filtering then $(Settings.hamletFile "filterformclear") else nulltemplate
      where u = (here, [])

-- | Add transaction form.
addform :: ViewData -> Hamlet AppRoute
addform vd = $(Settings.hamletFile "addform")
 where
  datehelp = "eg: 2010/7/20" :: String
  deschelp = "eg: supermarket (optional)" :: String
  date = "today" :: String
  descriptions = sort $ nub $ map tdescription $ jtxns $ j vd
  manyfiles = (length $ files $ j vd) > 1
  postingfields VD{j=j} n = $(Settings.hamletFile "postingfields")
   where
    numbered = (++ show n)
    acctvar = numbered "account"
    amtvar = numbered "amount"
    acctnames = sort $ journalAccountNamesUsed j
    (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,$(Settings.hamletFile "postingfieldsamount")
                     ,"eg: $6"
                     )
       | otherwise = ("From account" :: String
                     ,"eg: assets:bank:checking" :: String
                     ,nulltemplate
                     ,"" :: String
                     )

-- | Edit journal form.
editform :: ViewData -> Hamlet AppRoute
editform VD{j=j} = $(Settings.hamletFile "editform")
  where
    manyfiles = (length $ files j) > 1
    formathelp = helplink "file-format" "file format help"

-- | Import journal form.
importform :: Hamlet AppRoute
importform = $(Settings.hamletFile "importform")

journalselect :: [(FilePath,String)] -> Hamlet AppRoute
journalselect journalfiles = $(Settings.hamletFile "journalselect")

----------------------------------------------------------------------
-- utilities

nulltemplate :: Hamlet AppRoute
nulltemplate = [$hamlet||]

-- | A bundle of data useful for handlers and their templates.
data ViewData = VD {
     opts  :: [Opt]       -- ^ command-line options at startup
    ,a     :: String      -- ^ current a (query) parameter
    ,p     :: String      -- ^ current p (query) parameter
    ,q     :: String      -- ^ current q (query) parameter
    ,fspec :: FilterSpec  -- ^ a journal filter specification based on the above
    ,m     :: Matcher     -- ^ a search/filter expression based on the above
    ,j     :: Journal     -- ^ an up-to-date parsed journal
    ,today :: Day         -- ^ the current day
    ,here  :: AppRoute    -- ^ the current route
    ,msg   :: Maybe Html  -- ^ the current UI message if any, possibly from the current request
    }

mkvd :: ViewData
mkvd = VD {
      opts  = []
     ,a     = ""
     ,p     = ""
     ,q     = ""
     ,fspec = nullfilterspec
     ,m     = MatchOr []
     ,j     = nulljournal
     ,today = ModifiedJulianDay 0
     ,here  = RootR
     ,msg   = Nothing
     }

-- | Gather data useful for a hledger-web request handler and its templates.
getViewData :: Handler ViewData
getViewData = do
  Just here'          <- getCurrentRoute
  (q, opts, fspec, m) <- getCurrentParameters
  (j, err)            <- getCurrentJournal opts
  msg                 <- getMessageOr err
  today               <- liftIO getCurrentDay
  return mkvd{opts=opts, q=q, fspec=fspec, m=m, j=j, today=today, here=here', msg=msg}
    where
      -- | Get current report parameters for this request.
      getCurrentParameters :: Handler (String, [Opt], FilterSpec, Matcher)
      getCurrentParameters = do
          app <- getYesod
          t <- liftIO $ getCurrentLocalTime
          q <- unpack `fmap` fromMaybe "" <$> lookupGetParam "q"
          let opts = appOpts app -- ++ [Period p']
              args = appArgs app -- ++ words' a'
              fspec = optsToFilterSpec opts args t
              m = parseMatcher q
          return (q, opts, fspec, m)

      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getCurrentJournal :: [Opt] -> Handler (Journal, Maybe String)
      getCurrentJournal opts = do
        j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
        (jE, changed) <- liftIO $ journalReloadIfChanged opts j
        if not changed
         then return (j,Nothing)
         else case jE of
                Right j' -> do liftIO $ putValue "hledger" "journal" j'
                               return (j',Nothing)
                Left e  -> do setMessage $ "error while reading" {- ++ ": " ++ e-}
                              return (j, Just e)


parseMatcher :: String -> Matcher
parseMatcher s = MatchOr $ map (MatchAcct True) $ words' s

parseMatcher2 :: String -> Matcher
parseMatcher2 s = either (const (MatchOr [])) id $ runParser matcher () "" $ lexmatcher s

lexmatcher :: String -> [String]
lexmatcher s = words' s

matcher :: GenParser String () Matcher
matcher = undefined

-- | Get the message set by the last request, or the newer message provided, if any.
getMessageOr :: Maybe String -> Handler (Maybe Html)
getMessageOr mnewmsg = do
  oldmsg <- getMessage
  return $ maybe oldmsg (Just . toHtml) mnewmsg

accountNameToAccountRegex :: String -> String
accountNameToAccountRegex "" = ""
accountNameToAccountRegex a = printf "^%s(:|$)" a

accountRegexToAccountName :: String -> String
accountRegexToAccountName = gsubRegexPR "^\\^(.*?)\\(:\\|\\$\\)$" "\\1"

isAccountRegex  :: String -> Bool
isAccountRegex s = take 1 s == "^" && (take 5 $ reverse s) == ")$|:("

-- | Quote-aware version of words - don't split on spaces which are inside quotes.
words' :: String -> [String]
words' = fromparse . parsewith ((quotedPattern <|> pattern) `sepBy` many1 spacenonewline)
    where
      pattern = many (noneOf " \n\r\"")
      quotedPattern = between (oneOf "'\"") (oneOf "'\"") $ many $ noneOf "'\""

numbered = zip [1..]
