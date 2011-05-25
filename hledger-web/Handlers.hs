{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handlers where

import Control.Applicative ((<$>)) --, (<*>))
import Data.ByteString (ByteString)
import Data.Text(Text,pack,unpack)
import System.FilePath (takeFileName, (</>))
import System.IO.Storage (putValue, getValue)
import Text.Hamlet hiding (hamletFile)
import Text.ParserCombinators.Parsec hiding (string)

import Hledger.Cli.Balance
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Cli.Version (version)
import Hledger.Data hiding (insert, today)

import App
import Settings
import StaticFiles


----------------------------------------------------------------------
-- handlers/views
----------------------------------------------------------------------

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" $ Settings.staticdir </> "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

----------------------------------------------------------------------

getRootR :: Handler RepHtml
getRootR = redirect RedirectTemporary defaultroute where defaultroute = JournalR

----------------------------------------------------------------------

-- | The main journal view, with accounts sidebar.
getJournalR :: Handler RepHtml
getJournalR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  -- app <- getYesod
  -- t <- liftIO $ getCurrentLocalTime
  let -- args = appArgs app
      -- fspec' = optsToFilterSpec opts args t
      sidecontent = balanceReportAsHtml opts td $ balanceReport opts fspec j
      maincontent = journalReportAsHtml opts td $ journalReport opts fspec j
      td = mktd{here=here, title="hledger journal", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet $(Settings.hamletFile "journal")

-- postJournalR :: Handler RepPlain
-- postJournalR = postJournalOnlyR

----------------------------------------------------------------------

-- | The main register view, with accounts sidebar.
getRegisterR :: Handler RepHtml
getRegisterR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  -- app <- getYesod
  -- t <- liftIO $ getCurrentLocalTime
  let -- args = appArgs app
      -- opts' = Empty:opts
      -- fspec' = optsToFilterSpec opts' args t
      sidecontent = balanceReportAsHtml opts td $ balanceReport opts fspec j
      maincontent = registerReportAsHtml opts td $ registerReport opts fspec j
      td = mktd{here=here, title="hledger register", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td
  defaultLayout $ do
      setTitle "hledger-web register"
      addHamlet $(Settings.hamletFile "register")

-- postRegisterR :: Handler RepPlain
-- postRegisterR = postJournalOnlyR

----------------------------------------------------------------------

-- | A simple accounts view, like hledger balance.
getAccountsOnlyR :: Handler RepHtml
getAccountsOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger accounts", msg=msg, a=a, p=p, j=j, today=today}
  defaultLayout $ do
      setTitle "hledger-web accounts"
      addHamlet $ balanceReportAsHtml opts td $ balanceReport opts fspec j

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> TemplateData -> BalanceReport -> Hamlet AppRoute
balanceReportAsHtml _ td@TD{here=here,a=a,p=p} (items,total) = $(Settings.hamletFile "balancereport")
 where
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
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml TD{p=p} (acct, adisplay, adepth, abal) = $(Settings.hamletFile "balancereportitem")
     where
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       acctpat = accountNameToAccountRegex acct
       pparam = if null p then "" else "&p="++p

accountNameToAccountRegex :: String -> String
accountNameToAccountRegex "" = ""
accountNameToAccountRegex a = printf "^%s(:|$)" a

accountRegexToAccountName :: String -> String
accountRegexToAccountName = gsubRegexPR "^\\^(.*?)\\(:\\|\\$\\)$" "\\1"

isAccountRegex  :: String -> Bool
isAccountRegex s = take 1 s == "^" && (take 5 $ reverse s) == ")$|:("

----------------------------------------------------------------------

-- | A simple journal view, like hledger print (with editing.)
getJournalOnlyR :: Handler RepHtml
getJournalOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger journal", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td
      txns = journalReportAsHtml opts td $ journalReport opts fspec j
  defaultLayout $ do
      setTitle "hledger-web journal only"
      addHamlet $(Settings.hamletFile "journalonly")

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> TemplateData -> JournalReport -> Hamlet AppRoute
journalReportAsHtml _ td items = $(Settings.hamletFile "journalreport")
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, JournalReportItem) -> Hamlet AppRoute
   itemAsHtml _ (n, t) = $(Settings.hamletFile "journalreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

addform :: TemplateData -> Hamlet AppRoute
addform td = $(Settings.hamletFile "addform")
 where
  -- datehelplink = helplink "dates" "..."
  datehelp = "eg: 2010/7/20" :: String
  deschelp = "eg: supermarket (optional)" :: String
  date = "today" :: String
  descriptions = sort $ nub $ map tdescription $ jtxns $ j td
  manyfiles = (length $ files $ j td) > 1

postingfields :: TemplateData -> Int -> Hamlet AppRoute
postingfields TD{j=j} n = $(Settings.hamletFile "postingfields")
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

editform :: TemplateData -> Hamlet AppRoute
editform TD{j=j} = $(Settings.hamletFile "editform")
  where
    manyfiles = (length $ files j) > 1
    formathelp = helplink "file-format" "file format help"

journalselect :: [(FilePath,String)] -> Hamlet AppRoute
journalselect journalfiles = $(Settings.hamletFile "journalselect")

importform :: Hamlet AppRoute
importform = $(Settings.hamletFile "importform")

{-
postJournalOnlyR :: Handler RepPlain
postJournalOnlyR = do
  action <- runFormPost' $ maybeStringInput "action"
  case action of Just "edit"   -> postEditForm
                 Just "import" -> postImportForm
                 _             -> postAddForm

-- | Handle a journal add form post.
postAddForm :: Handler RepPlain
postAddForm = do
  (_, _, _, _, j, _, _) <- getHandlerData
  today <- liftIO getCurrentDay
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

-- | Handle a journal edit form post.
postEditForm :: Handler RepPlain
postEditForm = do
  (_, _, _, _, j, _, _) <- getHandlerData
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

-- | Handle an import page post.
postImportForm :: Handler RepPlain
postImportForm = do
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
-}

----------------------------------------------------------------------

-- | A simple postings view like hledger register.
getRegisterOnlyR :: Handler RepHtml
getRegisterOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger register", msg=msg, a=a, p=p, j=j, today=today}
  hamletToRepHtml $ hledgerLayout td $ registerReportAsHtml opts td $ registerReport opts fspec j

-- | Render a register report as HTML.
registerReportAsHtml :: [Opt] -> TemplateData -> RegisterReport -> Hamlet AppRoute
registerReportAsHtml _ td items = $(Settings.hamletFile "registerreport")
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, RegisterReportItem) -> Hamlet AppRoute
   itemAsHtml TD{here=here,p=p} (n, (ds, posting, b)) = $(Settings.hamletFile "registerreportitem")
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

----------------------------------------------------------------------
-- common templates, helpers, utilities
----------------------------------------------------------------------

-- | Wrap a template with the standard hledger web ui page layout.
hledgerLayout :: TemplateData -> Hamlet AppRoute -> Hamlet AppRoute
hledgerLayout td@TD{title=basetitle, msg=msg, p=p, j=j, today=today} content =
 $(Settings.hamletFile "hledger-layout")
 where title' = basetitle ++ " - " ++ journaltitle
       (journaltitle, _) = journalTitleDesc j p today
       metacontent = "text/html; charset=utf-8" :: String
       m = fromMaybe "" msg

-- | Global toolbar/heading area.
navbar :: TemplateData -> Hamlet AppRoute
navbar TD{p=p,j=j,today=today} = $(Settings.hamletFile "navbar")
  where (title, desc) = journalTitleDesc j p today

-- | Links to the main views.
navlinks :: TemplateData -> Hamlet AppRoute
navlinks td = $(Settings.hamletFile "navlinks")
 where
   accountsjournallink  = navlink td "journal" JournalR
   accountsregisterlink = navlink td "register" RegisterR
   navlink :: TemplateData -> String -> AppRoute -> Hamlet AppRoute
   navlink TD{here=here,a=a,p=p} s dest = $(Settings.hamletFile "navlink")
    where u = (dest, concat [(if null a then [] else [("a", pack a)])
                            ,(if null p then [] else [("p", pack p)])])
          style | dest == here = "navlinkcurrent"
                | otherwise    = "navlink" :: Text

editlinks :: Hamlet AppRoute
editlinks = $(Settings.hamletFile "editlinks")

-- | Link to a topic in the manual.
helplink :: String -> String -> Hamlet AppRoute
helplink topic label = $(Settings.hamletFile "helplink")
    where u = manualurl ++ if null topic then "" else '#':topic

-- | Form controlling journal filtering parameters.
filterform :: TemplateData -> Hamlet AppRoute
filterform TD{here=here,a=a,p=p} = $(Settings.hamletFile "filterform")
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  filtering = not $ null a
  filteringperiod = not $ null p
  visible = "block" :: String
  filteringclass = if filtering then "filtering" else "" :: String
  filteringperiodclass = if filteringperiod then "filtering" else "" :: String
  stopfiltering = if filtering then $(Settings.hamletFile "filterformclear") else nulltemplate
      where u = (here, if filteringperiod then [("p", pack p)] else [])
  stopfilteringperiod = if filteringperiod then $(Settings.hamletFile "filterformclear") else nulltemplate
      where u = (here, if filtering then [("a", pack a)] else [])

nulltemplate :: Hamlet AppRoute
nulltemplate = [$hamlet||]

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

-- | A bundle of useful data passed to templates.
data TemplateData = TD {
     here         :: AppRoute -- ^ the current page's route
    ,title        :: String             -- ^ page's title
    ,msg          :: Maybe Html         -- ^ transient message
    ,a            :: String             -- ^ a (acct/desc filter pattern) parameter
    ,p            :: String             -- ^ p (period expression) parameter
    ,j            :: Journal            -- ^ the current journal
    ,today        :: Day                -- ^ the current day
    }

mktd :: TemplateData
mktd = TD {
      here = RootR
     ,title = "hledger"
     ,msg = Nothing
     ,a = ""
     ,p = ""
     ,j = nulljournal
     ,today = ModifiedJulianDay 0
     }

-- | Gather the data useful for a hledger web request handler, including:
-- initial command-line options, current a and p query string values, a
-- journal filter specification based on the above and the current time,
-- an up-to-date parsed journal, the current route, and the current ui
-- message if any.
getHandlerData :: Handler (String, String, [Opt], FilterSpec, Journal, Maybe Html, AppRoute)
getHandlerData = do
  Just here' <- getCurrentRoute
  (a, p, opts, fspec) <- getReportParameters
  (j, err) <- getLatestJournal opts
  msg <- getMessage' err
  return (a, p, opts, fspec, j, msg, here')
    where
      -- | Get current report parameters for this request.
      getReportParameters :: Handler (String, String, [Opt], FilterSpec)
      getReportParameters = do
          app <- getYesod
          t <- liftIO $ getCurrentLocalTime
          a <- fromMaybe "" <$> lookupGetParam "a"
          p <- fromMaybe "" <$> lookupGetParam "p"
          let (a',p') = (unpack a, unpack p)
              opts = appOpts app ++ [Period p']
              args = appArgs app ++ words' a'
              fspec = optsToFilterSpec opts args t
          return (a', p', opts, fspec)

      -- | Quote-sensitive words, ie don't split on spaces which are inside quotes.
      words' :: String -> [String]
      words' = fromparse . parsewith ((quotedPattern <|> pattern) `sepBy` many1 spacenonewline)
          where
            pattern = many (noneOf " \n\r\"")
            quotedPattern = between (oneOf "'\"") (oneOf "'\"") $ many $ noneOf "'\""

      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getLatestJournal :: [Opt] -> Handler (Journal, Maybe String)
      getLatestJournal opts = do
        j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
        (jE, changed) <- liftIO $ journalReloadIfChanged opts j
        if not changed
         then return (j,Nothing)
         else case jE of
                Right j' -> do liftIO $ putValue "hledger" "journal" j'
                               return (j',Nothing)
                Left e  -> do setMessage $ "error while reading" {- ++ ": " ++ e-}
                              return (j, Just e)

      -- | Helper to work around a yesod feature (can't set and get a message in the same request.)
      getMessage' :: Maybe String -> Handler (Maybe Html)
      getMessage' newmsgstr = do
        oldmsg <- getMessage
        return $ maybe oldmsg (Just . toHtml) newmsgstr

