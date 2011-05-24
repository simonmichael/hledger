{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handlers where

import Control.Applicative ((<$>)) --, (<*>))
import Data.Text(Text,pack,unpack)
import System.FilePath (takeFileName) --(</>))
import System.IO.Storage (putValue, getValue)
import Text.Hamlet
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

getRootR :: Handler RepHtml
getRootR = redirect RedirectTemporary defaultroute where defaultroute = JournalR
    -- defaultLayout $ do
    --     h2id <- lift newIdent
    --     setTitle "hledger-web homepage"
    --     addWidget $(widgetFile "homepage")

----------------------------------------------------------------------

-- | A combined accounts and journal view.
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
  hamletToRepHtml $ pageLayout td [hamlet|
 <div#content
  <div#sidebar
   ^{sidecontent}
  <div#main.journal
   ^{navlinks td}
   <div#transactions
    ^{filterform td}
    ^{maincontent}
   ^{addform td}
   ^{editform'}
   ^{importform}
|]

-- postJournalR :: Handler RepPlain
-- postJournalR = postJournalOnlyR

----------------------------------------------------------------------

-- | A combined accounts and register view.
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
  hamletToRepHtml $ pageLayout td [hamlet|
 <div#content
  <div#sidebar
   ^{sidecontent}
  <div#main.journal
   ^{navlinks td}
   <div#transactions
    ^{filterform td}
    ^{maincontent}
   ^{addform td}
   ^{editform'}
   ^{importform}
|]

-- postRegisterR :: Handler RepPlain
-- postRegisterR = postJournalOnlyR

----------------------------------------------------------------------

-- | A simple accounts and balances view like hledger balance.
getAccountsOnlyR :: Handler RepHtml
getAccountsOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger accounts", msg=msg, a=a, p=p, j=j, today=today}
  hamletToRepHtml $ pageLayout td $ balanceReportAsHtml opts td $ balanceReport opts fspec j

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> TemplateData -> BalanceReport -> Hamlet AppRoute
balanceReportAsHtml _ td@TD{here=here,a=a,p=p} (items,total) = [hamlet|
^{accountsheading}
<table.balancereport>
 $forall i <- items
  ^{itemAsHtml' i}
 <tr.totalrule>
  <td colspan=2>
 <tr>
  <td>
  <td align=right>#{mixedAmountAsHtml total}
|]
 where
   accountsheading = [hamlet|
                      <span#accountsheading
                       accounts
                       \ #
                       ^{showlinks}
                      |] :: Hamlet AppRoute
       where
         filteringaccts = not $ null a
         showlinks = [hamlet|<span#showmoreaccounts>^{showmore} ^{showall}|] :: Hamlet AppRoute
         showmore = case (filteringaccts, items) of
                      -- cunning parent account logic
                      (True, ((acct, _, _, _):_)) ->
                          let a' = if isAccountRegex a then a else acct
                              a'' = accountNameToAccountRegex $ parentAccountName $ accountRegexToAccountName a'
                              parenturl = (here, [("a",pack a''), ("p",pack p)])
                          in [hamlet|
                              \ | #
                              <a href=@?{parenturl}>show more &uarr;
                              |]
                      _ -> nulltemplate
         showall = if filteringaccts
                    then [hamlet|
                          \ | #
                          <a href=@?{allurl}>show all
                          |]
                    else nulltemplate
             where allurl = (here, [("p",pack p)])
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml TD{p=p} (acct, adisplay, adepth, abal) = [hamlet|
     <tr.item
      <td.account
       #{indent}
       <a href="@{here}?a=#{acctpat}#{pparam}">#{adisplay}
      <td.balance align=right>#{mixedAmountAsHtml abal}
     |] where
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

-- | A basic journal view, like hledger print, with editing.
getJournalOnlyR :: Handler RepHtml
getJournalOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger journal", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td
      txns = journalReportAsHtml opts td $ journalReport opts fspec j
  hamletToRepHtml $ pageLayout td [hamlet|
<div#journal
 <div.nav2
  <a#addformlink href onclick="return addformToggle(event)" add one transaction
  \ | #
  <a#editformlink href onclick="return editformToggle(event)" edit the whole journal
 <div#transactions ^{txns}
 ^{addform td}
 ^{editform'}
|]

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> TemplateData -> JournalReport -> Hamlet AppRoute
journalReportAsHtml _ td items = [hamlet|
<table.journalreport>
 $forall i <- number items
  ^{itemAsHtml' i}
|]
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, JournalReportItem) -> Hamlet AppRoute
   itemAsHtml _ (n, t) = [hamlet|
     <tr.item.#{evenodd} >
      <td.transaction>
       <pre> #{txn}
     |] where
       evenodd = if even n then "even" else "odd" :: String
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

addform :: TemplateData -> Hamlet AppRoute
addform td = [hamlet|
<script type=text/javascript>
 $(document).ready(function() {
    /* dhtmlxcombo setup */
    window.dhx_globalImgPath="../static/";
    var desccombo  = new dhtmlXCombo("description");
    var acct1combo = new dhtmlXCombo("account1");
    var acct2combo = new dhtmlXCombo("account2");
    desccombo.enableFilteringMode(true);
    acct1combo.enableFilteringMode(true);
    acct2combo.enableFilteringMode(true);
    desccombo.setSize(300);
    acct1combo.setSize(300);
    acct2combo.setSize(300);
    /* desccombo.enableOptionAutoHeight(true, 20); */
    /* desccombo.setOptionHeight(200); */
 });
<form#addform method=POST style=display:none;
  <table.form
   <tr
    <td colspan=4
     <table
      <tr#descriptionrow
       <td
        Date:
       <td
        <input.textinput size=15 name=date value=#{date}
       <td style=padding-left:1em;
        Description:
       <td
        <select id=description name=description
         <option
         $forall d <- descriptions
          <option value=#{d}>#{d}
      <tr.helprow
       <td
       <td
        <span.help>#{datehelp} #
       <td
       <td
        <span.help>#{deschelp}
   ^{postingsfields td}
   <tr#addbuttonrow
    <td colspan=4
     <input type=hidden name=action value=add
     <input type=submit name=submit value="add transaction"
     $if manyfiles
      \ to: ^{journalselect $ files $ j td}
|]
 where
  -- datehelplink = helplink "dates" "..."
  datehelp = "eg: 2010/7/20" :: String
  deschelp = "eg: supermarket (optional)" :: String
  date = "today" :: String
  descriptions = sort $ nub $ map tdescription $ jtxns $ j td
  manyfiles = (length $ files $ j td) > 1

postingsfields :: TemplateData -> Hamlet AppRoute
postingsfields td = [hamlet|
 ^{p1}
 ^{p2}
|]
  where
    p1 = postingfields td 1
    p2 = postingfields td 2

postingfields :: TemplateData -> Int -> Hamlet AppRoute
postingfields TD{j=j} n = [hamlet|
 <tr#postingrow
  <td align=right>#{acctlabel}:
  <td
   <select id=#{acctvar} name=#{acctvar}
    <option
    $forall a <- acctnames
     <option value=#{a}>#{a}
  ^{amtfield}
 <tr.helprow
  <td
  <td
   <span.help>#{accthelp}
  <td
  <td
   <span.help>#{amthelp}
|]
 where
  numbered = (++ show n)
  acctvar = numbered "account"
  amtvar = numbered "amount"
  acctnames = sort $ journalAccountNamesUsed j
  (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,[hamlet|
                       <td style=padding-left:1em;
                        Amount:
                       <td
                        <input.textinput size=15 name=#{amtvar} value=""
                       |]
                     ,"eg: $6"
                     )
       | otherwise = ("From account" :: String
                     ,"eg: assets:bank:checking" :: String
                     ,nulltemplate
                     ,"" :: String
                     )

editform :: TemplateData -> Hamlet AppRoute
editform TD{j=j} = [hamlet|
 <form#editform method=POST style=display:none;
  <table.form#editform
   $if manyfiles
    <tr
     <td colspan=2
      Editing ^{journalselect $ files j}
   <tr
    <td colspan=2
     $forall f <- files j
      <textarea id=#{fst f}_textarea name=text rows=25 cols=80 style=display:none; disabled=disabled
       #{snd f}
   <tr#addbuttonrow
    <td
     <span.help ^{formathelp}
    <td align=right
     <span.help Are you sure ? This will overwrite the journal. #
     <input type=hidden name=action value=edit
     <input type=submit name=submit value="save journal"
     \ or #
     <a href onclick="return editformToggle(event)">cancel
|] -- XXX textarea ids are unquoted journal file paths, which is not valid html
  where
    manyfiles = (length $ files j) > 1
    formathelp = helplink "file-format" "file format help"

journalselect :: [(FilePath,String)] -> Hamlet AppRoute
journalselect journalfiles = [hamlet|
     <select id=journalselect name=journal onchange="editformJournalSelect(event)"
      $forall f <- journalfiles
       <option value=#{fst f}>#{fst f}
|]

importform :: Hamlet AppRoute
importform = [hamlet|
 <form#importform method=POST style=display:none;
  <table.form
   <tr
    <td
     <input type=file name=file
     <input type=hidden name=action value=import
     <input type=submit name=submit value="import from file"
     \ or #
     <a href onclick="return importformToggle(event)" cancel
|]

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
  hamletToRepHtml $ pageLayout td $ registerReportAsHtml opts td $ registerReport opts fspec j

-- | Render a register report as HTML.
registerReportAsHtml :: [Opt] -> TemplateData -> RegisterReport -> Hamlet AppRoute
registerReportAsHtml _ td items = [hamlet|
<table.registerreport
 <tr.headings
  ^{headings}
 $forall i <- number items
  ^{itemAsHtml' i}
|]
 where
   number = zip [1..]
   headings = [hamlet|
               <th.date align=left Date
               <th.description align=left Description
               <th.account align=left Account
               <th.amount align=right Amount
               <th.balance align=right Balance
               |] :: Hamlet AppRoute
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, RegisterReportItem) -> Hamlet AppRoute
   itemAsHtml TD{here=here,p=p} (n, (ds, posting, b)) = [hamlet|
     <tr.item.#{evenodd}.#{firstposting}
      <td.date>#{date}
      <td.description>#{desc}
      <td.account
       <a href="@{here}?a=#{acctpat}#{pparam}">#{acct}
      <td.amount align=right>#{mixedAmountAsHtml $ pamount posting}
      <td.balance align=right>#{mixedAmountAsHtml b}
     |] where
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
-- utilities, common templates
----------------------------------------------------------------------

nulltemplate :: Hamlet AppRoute
nulltemplate = [hamlet||]

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

-- | Wrap a template with the standard hledger web ui page layout.
pageLayout :: TemplateData -> Hamlet AppRoute -> Hamlet AppRoute
pageLayout td@TD{title=basetitle, msg=msg, p=p, j=j, today=today} content = [hamlet|
!!!
<html
 <head
  <title>#{title'}
  <meta http-equiv=Content-Type content=#{metacontent}
  <script type=text/javascript src=@{StaticR jquery_js}
  <script type=text/javascript src=@{StaticR jquery_url_js}
  <script type=text/javascript src=@{StaticR dhtmlxcommon_js}
  <script type=text/javascript src=@{StaticR dhtmlxcombo_js}
  <script type=text/javascript src=@{StaticR hledger_js}
  <link rel=stylesheet type=text/css media=all href=@{StaticR style_css}
 <body
  ^{navbar td}
  <div#messages>#{m}
  <div#content
   ^{content}
|]
 where title' = basetitle ++ " - " ++ journaltitle
       (journaltitle, _) = journalTitleDesc j p today
       metacontent = "text/html; charset=utf-8" :: String
       m = fromMaybe "" msg

-- | Global toolbar/heading area.
navbar :: TemplateData -> Hamlet AppRoute
navbar TD{p=p,j=j,today=today} = [hamlet|
 <div#navbar
  <a.topleftlink href=#{hledgerorgurl}
   hledger-web
   <br />
   #{version}
  <a.toprightlink href=#{manualurl} target=hledgerhelp>manual
  <h1>#{title}
  \ #
  <span#journaldesc>#{desc}
|]
  where (title, desc) = journalTitleDesc j p today

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

-- | Links to the main views.
navlinks :: TemplateData -> Hamlet AppRoute
navlinks td = [hamlet|
 <div#navlinks
  ^{accountsjournallink}
  \ | #
  ^{accountsregisterlink}
  \ | #
  <a#addformlink href onclick="return addformToggle(event)">add transaction
  <a#importformlink href onclick="return importformToggle(event)" style="display:none;">import transactions
  \ | #
  <a#editformlink href onclick="return editformToggle(event)">edit journal
|]
--  \ | #
 where
   accountsjournallink  = navlink td "journal" JournalR
   accountsregisterlink = navlink td "register" RegisterR

navlink :: TemplateData -> String -> AppRoute -> Hamlet AppRoute
navlink TD{here=here,a=a,p=p} s dest = [hamlet|<a##{s}link.#{style} href=@?{u}>#{s}|]
 where u = (dest, concat [(if null a then [] else [("a", pack a)])
                         ,(if null p then [] else [("p", pack p)])])
       style | dest == here = "navlinkcurrent"
             | otherwise    = "navlink" :: Text

-- | Form controlling journal filtering parameters.
filterform :: TemplateData -> Hamlet AppRoute
filterform TD{here=here,a=a,p=p} = [hamlet|
 <div#filterformdiv
  <form#filterform.form method=GET style=display:#{visible};
   <table.form
    <tr.#{filteringperiodclass}
     <td
      filter by period:
      \ #
     <td
      <input name=p size=60 value=#{p}
      ^{phelp}
      \ #
     <td align=right
      ^{stopfilteringperiod}
    <tr.#{filteringclass}
     <td
      filter by account/description:
      \ #
     <td
      <input name=a size=60 value=#{a}
      ^{ahelp}
      \ #
      <input type=submit value=filter #
      \ #
     <td align=right
      ^{stopfiltering}
|]
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  filtering = not $ null a
  filteringperiod = not $ null p
  visible = "block" :: String
  filteringclass = if filtering then "filtering" else "" :: String
  filteringperiodclass = if filteringperiod then "filtering" else "" :: String
  stopfiltering = if filtering then [hamlet|<a#stopfilterlink href=@?{u}>clear filter|] else nulltemplate
      where u = (here, if filteringperiod then [("p", pack p)] else [])
  stopfilteringperiod = if filteringperiod then [hamlet|<a#stopfilterlink href=@?{u}>clear filter|] else nulltemplate
      where u = (here, if filtering then [("a", pack a)] else [])

-- | Link to a topic in the manual.
helplink :: String -> String -> Hamlet AppRoute
helplink topic label = [hamlet|<a href=#{u} target=hledgerhelp>#{label}|]
    where u = manualurl ++ if null topic then "" else '#':topic

