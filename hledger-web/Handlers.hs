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
import Safe
import System.FilePath (takeFileName, (</>))
import System.IO.Storage (putValue, getValue)
import Text.Hamlet hiding (hamletFile)
import Text.Printf
import Text.RegexPR
import Yesod.Form
import Yesod.Json

import Hledger.Cli
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
  vd@VD{opts=opts,m=m,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml opts vd{q=""} $ balanceReport opts nullfilterspec j
      maincontent = journalReportAsHtml opts vd $ journalReport opts nullfilterspec $ filterJournalTransactions2 m j
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet $(Settings.hamletFile "journal")

postJournalR :: Handler RepPlain
postJournalR = handlePost

-- | The main register view, with accounts sidebar.
getRegisterR :: Handler RepHtml
getRegisterR = do
  vd@VD{opts=opts,m=m,j=j} <- getViewData
  let sidecontent = balanceReportAsHtml  opts vd{q=""} $ balanceReport  opts nullfilterspec j
      maincontent = registerReportAsHtml opts vd $ registerReport opts nullfilterspec $ filterJournalPostings2 m j
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
  vd@VD{opts=opts,m=m,j=j} <- getViewData
  defaultLayout $ do
      setTitle "hledger-web register only"
      addHamlet $ registerReportAsHtml opts vd $ registerReport opts nullfilterspec $ filterJournalPostings2 m j

postRegisterOnlyR :: Handler RepPlain
postRegisterOnlyR = handlePost

-- | A simple accounts view, like hledger balance. If the Accept header
-- specifies json, returns the chart of accounts as json.
getAccountsR :: Handler RepHtmlJson
getAccountsR = do
  vd@VD{opts=opts,m=m,j=j} <- getViewData
  let j' = filterJournalPostings2 m j
      html = do
        setTitle "hledger-web accounts"
        addHamlet $ balanceReportAsHtml opts vd $ balanceReport opts nullfilterspec j'
      json = jsonMap [("accounts", toJSON $ journalAccountNames j')]
  defaultLayoutJson html json

-- | Return the chart of accounts as json, without needing a special Accept header.
getAccountsJsonR :: Handler RepJson
getAccountsJsonR = do
  VD{m=m,j=j} <- getViewData
  let j' = filterJournalPostings2 m j
  jsonToRepJson $ jsonMap [("accounts", toJSON $ journalAccountNames j')]

-- helpers

accountUrl :: String -> String
accountUrl a = "in:" ++ quoteIfSpaced (accountNameToAccountRegex a)

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> ViewData -> BalanceReport -> Hamlet AppRoute
balanceReportAsHtml _ vd@VD{here=here,q=q,m=m,j=j} (items,total) = $(Settings.hamletFile "balancereport")
 where
   filtering = not $ null q
   inacct = headMay $ filter (m `matchesInAccount`) $ journalAccountNames j
   itemAsHtml :: ViewData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml VD{here=here,q=q} (acct, adisplay, aindent, abal) = $(Settings.hamletFile "balancereportitem")
     where
       depthclass = "depth"++show aindent
       inclass = if acct == inacct then "inacct" else "notinacct" :: String
       indent = preEscapedString $ concat $ replicate (2 * aindent) "&nbsp;"
       accturl = (here, [("q", pack $ accountUrl acct)])

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
   itemAsHtml VD{here=here} (n, (ds, posting, b)) = $(Settings.hamletFile "registerreportitem")
     where
       evenodd = if even n then "even" else "odd" :: String
       (firstposting, date, desc) = case ds of Just (da, de) -> ("firstposting", show da, de)
                                               Nothing -> ("", "", "") :: (String,String,String)
       acct = paccount posting
       accturl = (here, [("q", pack $ accountUrl acct)])

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

-- | Links to navigate between the main views.
navlinks :: ViewData -> Hamlet AppRoute
navlinks vd = $(Settings.hamletFile "navlinks")
 where
   accountsjournallink  = navlink vd "transactions" JournalR
   accountsregisterlink = navlink vd "postings" RegisterR
   navlink :: ViewData -> String -> AppRoute -> Hamlet AppRoute
   navlink VD{here=here,q=q} s dest = $(Settings.hamletFile "navlink")
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

-- | Form controlling journal filtering parameters.
filterform :: ViewData -> Hamlet AppRoute
filterform VD{here=here,q=q} = $(Settings.hamletFile "filterform")
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
     opts  :: [Opt]       -- ^ command-line options at startup
    ,q     :: String      -- ^ current q (query) parameter
    ,m     :: Matcher     -- ^ a search/filter expression based on the above
    ,j     :: Journal     -- ^ the up-to-date parsed unfiltered journal
    ,today :: Day         -- ^ the current day
    ,here  :: AppRoute    -- ^ the current route
    ,msg   :: Maybe Html  -- ^ the current UI message if any, possibly from the current request
    }

mkvd :: ViewData
mkvd = VD {
      opts  = []
     ,q     = ""
     ,m     = MatchAny
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
  let m = strace $ parseMatcher today q
  return mkvd{opts=opts, q=q, m=m, j=j, today=today, here=here', msg=msg}
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

accountNameToAccountRegex :: String -> String
accountNameToAccountRegex "" = ""
accountNameToAccountRegex a = printf "^%s(:|$)" a

accountRegexToAccountName :: String -> String
accountRegexToAccountName = gsubRegexPR "^\\^(.*?)\\(:\\|\\$\\)$" "\\1"

isAccountRegex  :: String -> Bool
isAccountRegex s = take 1 s == "^" && (take 5 $ reverse s) == ")$|:("

numbered = zip [1..]
