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
-- import Safe
import System.FilePath (takeFileName, (</>))
import System.IO.Storage (putValue, getValue)
import Text.Hamlet hiding (hamletFile)
import Text.Printf
import Yesod.Form
import Yesod.Json

import Hledger.Cli
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
  vd@VD{opts=opts,m=m,am=am,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml opts vd $ balanceReport2 opts am j
      maincontent = journalReportAsHtml opts vd $ journalReport opts nullfilterspec $ filterJournalTransactions2 m j
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet $(Settings.hamletFile "journal")

postJournalR :: Handler RepPlain
postJournalR = handlePost

-- | The main register view, with accounts sidebar.
getRegisterR :: Handler RepHtml
getRegisterR = do
  vd@VD{opts=opts,qopts=qopts,m=m,am=am,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml opts vd $ balanceReport2 opts am j
      maincontent =
          case inAccountMatcher qopts of Just m' -> accountRegisterReportAsHtml opts vd $ accountRegisterReport opts j m m'
                                         Nothing -> accountRegisterReportAsHtml opts vd $ journalRegisterReport opts j m
      editform' = editform vd
  defaultLayout $ do
      setTitle "hledger-web register"
      addHamlet $(Settings.hamletFile "register")

postRegisterR :: Handler RepPlain
postRegisterR = handlePost

-- | A simple journal view, like hledger print (with editing.)
getJournalOnlyR :: Handler RepHtml
getJournalOnlyR = do
  vd@VD{opts=opts,m=m,j=j} <- getViewData
  defaultLayout $ do
      setTitle "hledger-web journal only"
      addHamlet $ journalReportAsHtml opts vd $ journalReport opts nullfilterspec $ filterJournalTransactions2 m j

postJournalOnlyR :: Handler RepPlain
postJournalOnlyR = handlePost

-- | A simple postings view, like hledger register (with editing.)
getRegisterOnlyR :: Handler RepHtml
getRegisterOnlyR = do
  vd@VD{opts=opts,qopts=qopts,m=m,j=j} <- getViewData
  defaultLayout $ do
      setTitle "hledger-web register only"
      addHamlet $
          case inAccountMatcher qopts of Just m' -> accountRegisterReportAsHtml opts vd $ accountRegisterReport opts j m m'
                                         Nothing -> accountRegisterReportAsHtml opts vd $ journalRegisterReport opts j m

postRegisterOnlyR :: Handler RepPlain
postRegisterOnlyR = handlePost

-- | A simple accounts view, like hledger balance. If the Accept header
-- specifies json, returns the chart of accounts as json.
getAccountsR :: Handler RepHtmlJson
getAccountsR = do
  vd@VD{opts=opts,m=m,am=am,j=j} <- getViewData
  let j' = filterJournalPostings2 m j
      html = do
        setTitle "hledger-web accounts"
        addHamlet $ balanceReportAsHtml opts vd $ balanceReport2 opts am j'
      json = jsonMap [("accounts", toJSON $ journalAccountNames j')]
  defaultLayoutJson html json

-- | Return the chart of accounts as json, without needing a special Accept header.
getAccountsJsonR :: Handler RepJson
getAccountsJsonR = do
  VD{m=m,j=j} <- getViewData
  let j' = filterJournalPostings2 m j
  jsonToRepJson $ jsonMap [("accounts", toJSON $ journalAccountNames j')]

-- helpers

accountQuery :: AccountName -> String
accountQuery a = "inacct:" ++ quoteIfSpaced a -- (accountNameToAccountRegex a)

accountsQuery :: AccountName -> String
accountsQuery a = "inaccts:" ++ quoteIfSpaced a -- (accountNameToAccountRegex a)

accountsOnlyQuery :: AccountName -> String
accountsOnlyQuery a = "inacctsonly:" ++ quoteIfSpaced a -- (accountNameToAccountRegex a)

-- accountUrl :: AppRoute -> AccountName -> (AppRoute,[(String,ByteString)])
accountUrl r a = (r, [("q",pack $ accountQuery a)])

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> ViewData -> BalanceReport -> Hamlet AppRoute
balanceReportAsHtml _ vd@VD{here=here,m=m,q=q,qopts=qopts,j=j} (items',total) = $(Settings.hamletFile "balancereport")
 where
   l = journalToLedger nullfilterspec j
   inacctmatcher = inAccountMatcher qopts
   showacctmatcher = showAccountMatcher qopts
   allaccts = isNothing inacctmatcher
   items = maybe items' (\m -> filter (matchesAccount m . \(a,_,_,_)->a) items') showacctmatcher
   itemAsHtml :: ViewData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml VD{here=here,q=q} (acct, adisplay, aindent, abal) = $(Settings.hamletFile "balancereportitem")
     where
       hassubs = not $ null $ ledgerSubAccounts l $ ledgerAccount l acct
       numpostings = length $ apostings $ ledgerAccount l acct
       depthclass = "depth"++show aindent
       inacctclass = case inacctmatcher of
                       Just m -> if m `matchesAccount` acct then "inacct" else "notinacct"
                       Nothing -> "" :: String
       indent = preEscapedString $ concat $ replicate (2 * aindent) "&nbsp;"
       acctquery = (RegisterR, [("q", pack $ accountQuery acct)])
       acctsquery = (RegisterR, [("q", pack $ accountsQuery acct)])
       acctsonlyquery = (RegisterR, [("q", pack $ accountsOnlyQuery acct)])

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> ViewData -> JournalReport -> Hamlet AppRoute
journalReportAsHtml _ vd items = $(Settings.hamletFile "journalreport")
 where
   itemAsHtml :: ViewData -> (Int, JournalReportItem) -> Hamlet AppRoute
   itemAsHtml _ (n, t) = $(Settings.hamletFile "journalreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

-- Account-specific transaction register, when an account is focussed.
accountRegisterReportAsHtml :: [Opt] -> ViewData -> AccountRegisterReport -> Hamlet AppRoute
accountRegisterReportAsHtml _ vd (balancelabel,items) = $(Settings.hamletFile "accountregisterreport")
 where
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, AccountRegisterReportItem) -> Hamlet AppRoute
   itemAsHtml VD{here=here,p=p} (n, newd, newm, newy, (t, t', split, acct, amt, bal)) = $(Settings.hamletFile "accountregisterreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       datetransition | newm = "newmonth"
                      | newd = "newday"
                      | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate t, tdescription t)
       acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)
       displayclass = if p then "" else "hidden" :: String

stringIfLongerThan :: Int -> String -> String
stringIfLongerThan n s = if length s > n then s else ""

numberAccountRegisterReportItems :: [AccountRegisterReportItem] -> [(Int,Bool,Bool,Bool,AccountRegisterReportItem)]
numberAccountRegisterReportItems [] = []
numberAccountRegisterReportItems is = number 0 nulldate is
  where
    number :: Int -> Day -> [AccountRegisterReportItem] -> [(Int,Bool,Bool,Bool,AccountRegisterReportItem)]
    number _ _ [] = []
    number n prevd (i@(Transaction{tdate=d},_,_,_,_,_):is)  = (n+1,newday,newmonth,newyear,i):(number (n+1) d is)
        where
          newday = d/=prevd
          newmonth = dm/=prevdm || dy/=prevdy
          newyear = dy/=prevdy
          (dy,dm,_) = toGregorian d
          (prevdy,prevdm,_) = toGregorian prevd

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
topbar VD{j=j,msg=msg,today=today} = $(Settings.hamletFile "topbar")
  where
    title = takeFileName $ journalFilePath j

-- -- | Links to navigate between the main views.
-- navlinks :: ViewData -> Hamlet AppRoute
-- navlinks vd = $(Settings.hamletFile "navlinks")
--  where
--    journallink  = navlink vd "transactions" JournalR
--    registerlink = navlink vd "postings" RegisterR

-- | Navigation link, preserving parameters and possibly highlighted.
navlink :: ViewData -> String -> AppRoute -> String -> Hamlet AppRoute
navlink VD{here=here,q=q} s dest title = $(Settings.hamletFile "navlink")
  where u = (dest, if null q then [] else [("q", pack q)])
        style | dest == here = "navlinkcurrent"
              | otherwise    = "navlink" :: Text

-- | Links to the various journal editing forms.
editlinks :: Hamlet AppRoute
editlinks = $(Settings.hamletFile "editlinks")

-- | Link to a topic in the manual.
helplink :: String -> String -> Hamlet AppRoute
helplink topic label = $(Settings.hamletFile "helplink")
    where u = manualurl ++ if null topic then "" else '#':topic

-- | Search form for entering custom queries to filter journal data.
searchform :: ViewData -> Hamlet AppRoute
searchform VD{here=here,q=q} = $(Settings.hamletFile "searchform")
 where
  filtering = not $ null q

-- | Add transaction form.
addform :: ViewData -> Hamlet AppRoute
addform vd = $(Settings.hamletFile "addform")
 where
  datehelp = "eg: 2010/7/20" :: String
  deschelp = "eg: supermarket (optional)" :: String
  date = "today" :: String
  descriptions = sort $ nub $ map tdescription $ jtxns $ j vd
  manyfiles = (length $ files $ j vd) > 1
  postingfields VD{j=j} n = $(Settings.hamletFile "addformpostingfields")
   where
    numbered = (++ show n)
    acctvar = numbered "account"
    amtvar = numbered "amount"
    acctnames = sort $ journalAccountNamesUsed j
    (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,$(Settings.hamletFile "addformpostingfieldsamount")
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
     opts  :: [Opt]         -- ^ command-line options at startup
    ,q     :: String        -- ^ current q parameter, the query expression
    ,p     :: Bool          -- ^ current p parameter, 1 or 0 shows/hides all postings, default is based on query
    ,m     :: Matcher       -- ^ a matcher parsed from the main query expr ("q" parameter)
    ,qopts :: [QueryOpt]    -- ^ query options parsed from the main query expr
    ,am    :: Matcher       -- ^ a matcher parsed from the accounts sidebar query expr ("a" parameter)
    ,aopts :: [QueryOpt]    -- ^ query options parsed from the accounts sidebar query expr
    ,j     :: Journal       -- ^ the up-to-date parsed unfiltered journal
    ,today :: Day           -- ^ the current day
    ,here  :: AppRoute      -- ^ the current route
    ,msg   :: Maybe Html    -- ^ the current UI message if any, possibly from the current request
    }

mkvd :: ViewData
mkvd = VD {
      opts  = []
     ,q     = ""
     ,p     = False
     ,m     = MatchAny
     ,qopts = []
     ,am     = MatchAny
     ,aopts = []
     ,j     = nulljournal
     ,today = ModifiedJulianDay 0
     ,here  = RootR
     ,msg   = Nothing
     }

-- | Gather data useful for a hledger-web request handler and its templates.
getViewData :: Handler ViewData
getViewData = do
  app        <- getYesod
  let opts = appOpts app
  (j, err)   <- getCurrentJournal opts
  msg        <- getMessageOr err
  Just here' <- getCurrentRoute
  today      <- liftIO getCurrentDay
  q          <- getParameter "q"
  let (querymatcher,queryopts) = parseQuery today q
  a          <- getParameter "a"
  let (acctsmatcher,acctsopts) = parseQuery today a
  p          <- getParameter "p"
  let p' | p == "1" = True
         | p == "0" = False
         | otherwise = isNothing $ inAccountMatcher queryopts
  return mkvd{opts=opts, q=q, p=p', m=querymatcher, qopts=queryopts, am=acctsmatcher, aopts=acctsopts, j=j, today=today, here=here', msg=msg}
    where
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

      -- | Get the named request parameter.
      getParameter :: String -> Handler String
      getParameter p = unpack `fmap` fromMaybe "" <$> lookupGetParam (pack p)

-- | Get the message set by the last request, or the newer message provided, if any.
getMessageOr :: Maybe String -> Handler (Maybe Html)
getMessageOr mnewmsg = do
  oldmsg <- getMessage
  return $ maybe oldmsg (Just . toHtml) mnewmsg

numbered = zip [1..]

