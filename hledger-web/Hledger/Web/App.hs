{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-| 
The web app providing a richer interface to hledger's data, along with
authentication, registration and persistent storage of user accounts.
-}
module Hledger.Web.App
    ( App (..)
    , withApp
    )
where
import Control.Applicative ((<$>), (<*>))
import Control.Failure
-- import qualified Data.ByteString.Lazy as L
import Data.Either
-- import System.Directory
import System.FilePath ((</>), takeFileName)
import System.IO.Storage (putValue, getValue)
import Text.ParserCombinators.Parsec hiding (string)

import Database.Persist.GenericSql (ConnectionPool, SqlPersist, runMigration, migrate)
import Yesod
import Yesod.Mail
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
-- import Yesod.WebRoutes
import Text.Hamlet (defaultHamletSettings)
import Text.Hamlet.RT

import Hledger.Cli.Commands.Add (appendToJournalFile)
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Cli.Version (version)
import Hledger.Data hiding (insert, today)
import Hledger.Read (journalFromPathAndString)
import Hledger.Read.Journal (someamount)
import Hledger.Web.Settings (
      withConnectionPool
    , runConnectionPool
    -- , staticroot
    , staticdir
    , hamletFile
    , cassiusFile
    , juliusFile
    , hledgerorgurl
    , manualurl
    , style_css
    , hledger_js
    , jquery_js
    , jquery_url_js
    , dhtmlxcommon_js
    , dhtmlxcombo_js
    , robots_txt
    )


----------------------------------------------------------------------
-- define the web app
----------------------------------------------------------------------

-- persistent data schema for the web app. User account info is stored here,
-- hledger's main data is stored in the usual places (journal files etc.)
-- persist (quasi-quoter from persistent) defines a list of data entities.
-- mkPersist (template haskell from persistent) defines persistence-capable data types based on these.
mkPersist [$persist|
User
    ident String
    password String null update
    UniqueUser ident
Email
    email String
    user UserId null update
    verkey String null update
    UniqueEmail email
|]

-- run-time data kept by the web app.
data App = App
    {appConnPool :: Maybe ConnectionPool
    ,appRoot    :: String
    ,appDataDir :: FilePath
    ,appOpts    :: [Opt]
    ,appArgs    :: [String]
    ,appJournal :: Journal
    ,appStatic  :: Static
    }

-- parseRoutes (quasi-quoter from web-routes) defines a list of route patterns for the web app.
-- mkYesod (template haskell from yesod) defines types for the web app based on the routes.
mkYesod "App" [$parseRoutes|
/auth            AuthR             Auth getAuth
/favicon.ico     FaviconR          GET
/robots.txt      RobotsR           GET
/static          StaticR           Static appStatic
/                IndexR            GET
/journalonly     JournalOnlyR      GET POST
/registeronly    RegisterOnlyR     GET
/accounts        AccountsOnlyR     GET
/journal         JournalR          GET POST
/register        RegisterR         GET POST
/addformrt       AddformRTR        GET
|]

type Handler = GHandler App App

instance Yesod App where
    approot = appRoot
    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addStyle $(cassiusFile "default-layout")
        hamletToRepHtml $(hamletFile "default-layout")
    authRoute _ = Just $ AuthR LoginR
    -- static file-serving optimisations, disable for the moment
    -- urlRenderOverride a (StaticR s) =
    --     Just $ uncurry (joinPath a staticroot) $ format s
    --   where
    --     format = formatPathSegments ss
    --     ss :: Site StaticRoute (String -> Maybe (GHandler Static App ChooseRep))
    --     ss = getSubSite
    urlRenderOverride _ _ = Nothing
    -- addStaticContent ext' _ content = do
    --     let fn = base64md5 content ++ '.' : ext'
    --     let statictmp = staticdir ++ "/tmp/"
    --     liftIO $ createDirectoryIfMissing True statictmp
    --     liftIO $ L.writeFile (statictmp ++ fn) content
    --     return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodPersist App where
    type YesodDB App = SqlPersist
    runDB db = do
      y <- getYesod
      let p = appConnPool y
      case p of Just p' -> runConnectionPool db p'
                Nothing -> error "no connection pool, programmer error" -- XXX

instance YesodAuth App where
    type AuthEntity App = User
    type AuthEmailEntity App = Email

    defaultDest _ = IndexR

    getAuthId creds _extra = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    openIdEnabled _ = True

    emailSettings _ = Just EmailSettings {
          addUnverified = \email verkey -> runDB $ insert $ Email email Nothing (Just verkey)
        , sendVerifyEmail = sendVerifyEmail'
        , getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
        , setVerifyKey = \eid key -> runDB $ update eid [EmailVerkey $ Just key]
        , verifyAccount = \eid -> runDB $ do
            me <- get eid
            case me of
                Nothing -> return Nothing
                Just e -> do
                    let email = emailEmail e
                    case emailUser e of
                        Just uid -> return $ Just uid
                        Nothing -> do
                            uid <- insert $ User email Nothing
                            update eid [EmailUser $ Just uid]
                            return $ Just uid
        , getPassword = runDB . fmap (join . fmap userPassword) . get
        , setPassword = \uid pass -> runDB $ update uid [UserPassword $ Just pass]
        , getEmailCreds = \email -> runDB $ do
            me <- getBy $ UniqueEmail email
            case me of
                Nothing -> return Nothing
                Just (eid, e) -> return $ Just EmailCreds
                    { emailCredsId = eid
                    , emailCredsAuthId = emailUser e
                    , emailCredsStatus = isJust $ emailUser e
                    , emailCredsVerkey = emailVerkey e
                    }
        , getEmail = runDB . fmap (fmap emailEmail) . get
        }

sendVerifyEmail' :: String -> String -> String -> GHandler Auth m ()
sendVerifyEmail' email _ verurl =
    liftIO $ renderSendMail Mail
            { mailHeaders =
                [ ("From", "noreply")
                , ("To", email)
                , ("Subject", "Verify your email address")
                ]
            , mailPlain = verurl
            , mailParts = return Part
                { partType = "text/html; charset=utf-8"
                , partEncoding = None
                , partDisposition = Inline
                , partContent = renderHamlet id [$hamlet|
%p Please confirm your email address by clicking on the link below.
%p
    %a!href=$verurl$ $verurl$
%p Thank you
|]
                }
            }

-- | Migrate the app's persistent data and run the given yesod/persistent/wai-ish IO action on it.
withApp :: App -> (Yesod.Application -> IO a) -> IO a
withApp app f = toPersistentApp app >>= toWaiApp >>= f

-- | Obtain a persistent db connection pool to the app, and run any necessary data migrations.
toPersistentApp :: App -> IO App
toPersistentApp app = withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ do
        migrate (undefined :: User)
        migrate (undefined :: Email)
        return ()
    return app{appConnPool=Just p}


----------------------------------------------------------------------
-- handler utilities, common templates
----------------------------------------------------------------------

nulltemplate = [$hamlet||]

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

mktd = TD {
      here = IndexR
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
  Just here <- getCurrentRoute
  (a, p, opts, fspec) <- getReportParameters
  (j, err) <- getLatestJournal opts
  msg <- getMessage' err
  return (a, p, opts, fspec, j, msg, here)
    where
      -- | Get current report parameters for this request.
      getReportParameters :: Handler (String, String, [Opt], FilterSpec)
      getReportParameters = do
          app <- getYesod
          t <- liftIO $ getCurrentLocalTime
          a <- fromMaybe "" <$> lookupGetParam "a"
          p <- fromMaybe "" <$> lookupGetParam "p"
          let opts = appOpts app ++ [Period p]
              args = appArgs app ++ words' a
              fspec = optsToFilterSpec opts args t
          return (a, p, opts, fspec)

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
                Left e  -> do setMessage $ string "error while reading" {- ++ ": " ++ e-}
                              return (j, Just e)

      -- | Helper to work around a yesod feature (can't set and get a message in the same request.)
      getMessage' :: Maybe String -> Handler (Maybe Html)
      getMessage' newmsgstr = do
        oldmsg <- getMessage
        return $ maybe oldmsg (Just . string) newmsgstr

-- | Wrap a template with the standard hledger web ui page layout.
pageLayout :: TemplateData -> Hamlet AppRoute -> Hamlet AppRoute
pageLayout td@TD{title=basetitle, msg=msg, p=p, j=j, today=today} content = [$hamlet|
!!!
%html
 %head
  %title $title'$
  %meta!http-equiv=Content-Type!content=$metacontent$
  %script!type=text/javascript!src=@StaticR.jquery_js@
  %script!type=text/javascript!src=@StaticR.jquery_url_js@
  %script!type=text/javascript!src=@StaticR.dhtmlxcommon_js@
  %script!type=text/javascript!src=@StaticR.dhtmlxcombo_js@
  %script!type=text/javascript!src=@StaticR.hledger_js@
  %link!rel=stylesheet!type=text/css!media=all!href=@StaticR.style_css@
 %body
  ^navbar.td^
  #messages $m$
  #content
   ^content^
|]
 where title' = basetitle ++ " - " ++ journaltitle
       (journaltitle, _) = journalTitleDesc j p today
       metacontent = "text/html; charset=utf-8"
       m = fromMaybe (string "") msg

-- | Global toolbar/heading area.
navbar :: TemplateData -> Hamlet AppRoute
navbar TD{p=p,j=j,today=today} = [$hamlet|
 #navbar
  %a.topleftlink!href=$hledgerorgurl$
   hledger
   <br />
   $version$
  %a.toprightlink!href=$manualurl$!target=hledgerhelp manual
  %h1 $title$
  \ $
  %span#journaldesc $desc$
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
navlinks td = [$hamlet|
 #navlinks
  ^accountsjournallink^
  \ | $
  ^accountsregisterlink^
  \ | $
  %a#addformlink!href!onclick="return addformToggle()" add transaction
  %a#importformlink!href!onclick="return importformToggle()"!style=display:none; import transactions
  \ | $
  %a#editformlink!href!onclick="return editformToggle()" edit journal
|]
--  \ | $
 where
   accountsjournallink  = navlink td "journal" JournalR
   accountsregisterlink = navlink td "register" RegisterR

navlink :: TemplateData -> String -> AppRoute -> Hamlet AppRoute
navlink TD{here=here,a=a,p=p} s dest = [$hamlet|%a#$s$link.$style$!href=@?u@ $s$|]
 where u = (dest, concat [(if null a then [] else [("a", a)])
                         ,(if null p then [] else [("p", p)])])
       style | dest == here = "navlinkcurrent"
             | otherwise    = "navlink"

-- | Form controlling journal filtering parameters.
filterform :: TemplateData -> Hamlet AppRoute
filterform TD{here=here,a=a,p=p} = [$hamlet|
 #filterformdiv
  %form#filterform.form!method=GET!style=display:$visible$;
   %table.form
    %tr.$filteringperiodclass$
     %td
      filter by period:
      \ $
     %td
      %input!name=p!size=60!value=$p$
      ^phelp^
      \ $
     %td!align=right
      ^stopfilteringperiod^
    %tr.$filteringclass$
     %td
      filter by account/description:
      \ $
     %td
      %input!name=a!size=60!value=$a$
      ^ahelp^
      \ $
      %input!type=submit!value=filter $
      \ $
     %td!align=right
      ^stopfiltering^
|]
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  filtering = not $ null a
  filteringperiod = not $ null p
  visible = "block"
  filteringclass = if filtering then "filtering" else ""
  filteringperiodclass = if filteringperiod then "filtering" else ""
  stopfiltering = if filtering then [$hamlet|%a#stopfilterlink!href=@?u@ clear filter|] else nulltemplate
      where u = (here, if filteringperiod then [("p", p)] else [])
  stopfilteringperiod = if filteringperiod then [$hamlet|%a#stopfilterlink!href=@?u@ clear filter|] else nulltemplate
      where u = (here, if filtering then [("a", a)] else [])

-- | Link to a topic in the manual.
helplink :: String -> String -> Hamlet AppRoute
helplink topic label = [$hamlet|%a!href=$u$!target=hledgerhelp $label$|]
    where u = manualurl ++ if null topic then "" else '#':topic

-- | Render a runtime template with the provided runtime data as html.
renderHamletFileRT :: FilePath -> HamletMap AppRoute -> Handler Html
renderHamletFileRT hfile hmap = do
  hrt <- readTemplateFile hfile >>= parseHamletRT defaultHamletSettings
  renderHamletRT hrt hmap urlParamsToString

-- | Read a file from the app's templates directory.
readTemplateFile :: FilePath -> Handler String
readTemplateFile hfile = do
  dir <- ((</> "templates") . appDataDir) `fmap` getYesod
  liftIO $ readFile $ dir </> hfile

-- what to do if rendering a runtime template fails.
instance Failure HamletException Handler
    where failure = error' . show

renderHamletAsHtml :: Hamlet AppRoute -> Html
renderHamletAsHtml h = h urlParamsToString

htmlAsHamlet :: Html -> Hamlet AppRoute
htmlAsHamlet h = [$hamlet|$h$|]

urlParamsToString :: AppRoute -> [(String,String)] -> String
urlParamsToString u [] = show u
urlParamsToString u ps = show u ++ "?" ++ intercalate "&" [k++"="++v | (k,v) <- ps]

-- | Convert a string to a hamlet HDHtml data item.
hdstring :: String -> HamletData AppRoute
hdstring = HDHtml . string

-- | Convert a simple list of strings to hamlet's complicated HDList type.
hdstringlist :: [String] -> HamletData AppRoute
hdstringlist ss = HDList [ [([], hdstring s)] | s <- ss ]

-- renderHamletRT' :: Failure HamletException m => HamletMap AppRoute -> HamletRT -> m Html
-- renderHamletRT' m h = renderHamletRT h m urlParamsToString

-- parseHamletRT' :: Failure HamletException m => String -> m HamletRT
-- parseHamletRT' s = parseHamletRT defaultHamletSettings s

-- hamletToHamletRT ::  Failure HamletException m => Hamlet AppRoute -> m HamletRT
-- hamletToHamletRT h = stringToHamletRT $ show $ unsafeByteString $ renderHamlet show h

----------------------------------------------------------------------
-- handlers/views
----------------------------------------------------------------------

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" $ staticdir </> "favicon.ico"

----------------------------------------------------------------------

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent robots_txt

----------------------------------------------------------------------

getIndexR :: Handler ()
getIndexR = redirect RedirectTemporary defaultroute where defaultroute = JournalR

----------------------------------------------------------------------

getDemoR :: Handler RepHtml
getDemoR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle $ string "hledger front page"
        addBody $(hamletFile "homepage")
        addStyle $(cassiusFile "homepage")
        addJavascript $(juliusFile "homepage")

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
  hamletToRepHtml $ pageLayout td [$hamlet|
 %div#content
  %div#sidebar
   ^sidecontent^
  %div#main.journal
   ^navlinks.td^
   ^addform.td^
   ^editform'^
   ^importform^
   ^filterform.td^
   ^maincontent^
|]

postJournalR :: Handler RepPlain
postJournalR = postJournalOnlyR

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
  hamletToRepHtml $ pageLayout td [$hamlet|
 %div#content
  %div#sidebar
   ^sidecontent^
  %div#main.journal
   ^navlinks.td^
   ^addform.td^
   ^editform'^
   ^importform^
   ^filterform.td^
   ^maincontent^
|]

postRegisterR :: Handler RepPlain
postRegisterR = postJournalOnlyR

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
balanceReportAsHtml _ td@TD{here=here,a=a,p=p} (items,total) = [$hamlet|
%table.balancereport
 %tr
  %td
   ^accountsheading^
 $forall items i
  ^itemAsHtml' i^
 %tr.totalrule
  %td!colspan=2
 %tr
  %td
  %td!align=right $mixedAmountAsHtml.total$
|]
 where
   accountsheading = [$hamlet|
                      accounts
                      \ $
                      %span#showmoreaccounts ^showmore^ ^showall^
                      <br />
                      <br />
                      |]
       where
         filteringaccts = not $ null a
         showmore = case (filteringaccts, items) of
                      -- cunning parent account logic
                      (True, ((acct, _, _, _):_)) ->
                          let a' = if isAccountRegex a then a else acct
                              a'' = accountNameToAccountRegex $ parentAccountName $ accountRegexToAccountName a'
                              parenturl = (here, [("a",a''), ("p",p)])
                          in [$hamlet|
                              \ | $
                              %a!href=@?parenturl@ show more &uarr;
                              |]
                      _ -> nulltemplate
         showall = if filteringaccts
                    then [$hamlet|
                          \ | $
                          %a!href=@?allurl@ show all
                          |]
                    else nulltemplate
             where allurl = (here, [("p",p)])
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet AppRoute
   itemAsHtml TD{p=p} (acct, adisplay, adepth, abal) = [$hamlet|
     %tr.item
      %td.account
       $indent$
       %a!href=$aurl$ $adisplay$
      %td.balance!align=right $mixedAmountAsHtml.abal$
     |] where
       -- current = if not (null a) && containsRegex a acct then "current" else ""
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       aurl = printf ".?a=%s%s" (accountNameToAccountRegex acct) p' :: String
       p' = if null p then "" else printf "&p=%s" p

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
  hamletToRepHtml $ pageLayout td [$hamlet|
%div#journal
 %div.nav2
  %a#addformlink!href!onclick="return addformToggle()" add one transaction
  \ | $
  %a#editformlink!href!onclick="return editformToggle()" edit the whole journal
 ^addform.td^
 ^editform'^
 #transactions ^txns^
|]

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> TemplateData -> JournalReport -> Hamlet AppRoute
journalReportAsHtml _ td items = [$hamlet|
%table.journalreport
 $forall number.items i
  ^itemAsHtml' i^
|]
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, JournalReportItem) -> Hamlet AppRoute
   itemAsHtml _ (n, t) = [$hamlet|
     %tr.item.$evenodd$
      %td.transaction
       %pre $txn$
     |] where
       evenodd = if even n then "even" else "odd"
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

addform :: TemplateData -> Hamlet AppRoute
addform td = [$hamlet|
%script!type=text/javascript
 $$(document).ready(function() {
    /* dhtmlxcombo setup */
    window.dhx_globalImgPath="../static/images/";
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
%form#addform!method=POST!style=display:none;
  %table.form
   %tr
    %td!colspan=4
     %table
      %tr#descriptionrow
       %td
        Date:
       %td
        %input.textinput!size=15!name=date!value=$date$
       %td!style=padding-left:1em;
        Description:
       %td
        %select!id=description!name=description
         %option
         $forall descriptions d
          %option!value=$d$ $d$
      %tr.helprow
       %td
       %td
        .help $datehelp$ $
       %td
       %td
        .help $deschelp$
   ^postingsfields.td^
   %tr#addbuttonrow
    %td!colspan=4
     %input!type=hidden!name=action!value=add
     %input!type=submit!name=submit!value="add transaction"
     $if manyfiles
      \ to: ^journalselect.files.j.td^
|]
 where
  -- datehelplink = helplink "dates" "..."
  datehelp = "eg: 2010/7/20"
  deschelp = "eg: supermarket (optional)"
  date = "today"
  descriptions = sort $ nub $ map tdescription $ jtxns $ j td
  manyfiles = (length $ files $ j td) > 1

postingsfields :: TemplateData -> Hamlet AppRoute
postingsfields td = [$hamlet|
 ^p1^
 ^p2^
|]
  where
    p1 = postingfields td 1
    p2 = postingfields td 2

postingfields :: TemplateData -> Int -> Hamlet AppRoute
postingfields TD{j=j} n = [$hamlet|
 %tr#postingrow
  %td!align=right $acctlabel$:
  %td
   %select!id=$acctvar$!name=$acctvar$
    %option
    $forall acctnames a
     %option!value=$a$ $a$
  ^amtfield^
 %tr.helprow
  %td
  %td
   .help $accthelp$
  %td
  %td
   .help $amthelp$
|]
 where
  numbered = (++ show n)
  acctvar = numbered "account"
  amtvar = numbered "amount"
  acctnames = sort $ journalAccountNamesUsed j
  (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,[$hamlet|
                       %td!style=padding-left:1em;
                        Amount:
                       %td
                        %input.textinput!size=15!name=$amtvar$!value=""
                       |]
                     ,"eg: $6"
                     )
       | otherwise = ("From account"
                     ,"eg: assets:bank:checking"
                     ,nulltemplate
                     ,""
                     )

editform :: TemplateData -> Hamlet AppRoute
editform TD{j=j} = [$hamlet|
 %form#editform!method=POST!style=display:none;
  %table.form#editform
   $if manyfiles
    %tr
     %td!colspan=2
      Editing ^journalselect.files.j^
   %tr
    %td!colspan=2
     $forall files.j f
      %textarea!id=$fst.f$_textarea!name=text!rows=25!cols=80!style=display:none;!disabled=disabled
       $snd.f$
   %tr#addbuttonrow
    %td
     %span.help ^formathelp^
    %td!align=right
     %span.help Are you sure ? This will overwrite the journal. $
     %input!type=hidden!name=action!value=edit
     %input!type=submit!name=submit!value="save journal"
     \ or $
     %a!href!onclick="return editformToggle()" cancel
|] -- XXX textarea ids are unquoted journal file paths, which is not valid html
  where
    manyfiles = (length $ files j) > 1
    formathelp = helplink "file-format" "file format help"

journalselect :: [(FilePath,String)] -> Hamlet AppRoute
journalselect journalfiles = [$hamlet|
     %select!id=journalselect!name=journal!onchange="editformJournalSelect()"
      $forall journalfiles f
       %option!value=$fst.f$ $fst.f$
|]

importform :: Hamlet AppRoute
importform = [$hamlet|
 %form#importform!method=POST!style=display:none;
  %table.form
   %tr
    %td
     %input!type=file!name=file
     %input!type=hidden!name=action!value=import
     %input!type=submit!name=submit!value="import from file"
     \ or $
     %a!href!onclick="return importformToggle()" cancel
|]

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
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today) dateM
      descE = Right $ fromMaybe "" descM
      acct1E = maybe (Left "to account required") Right acct1M
      acct2E = maybe (Left "from account required") Right acct2M
      amt1E = maybe (Left "amount required") (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx someamount) amt1M
      amt2E = maybe (Right missingamt)       (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx someamount) amt2M
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> if f `elem` journalFilePaths j
                              then Right f
                              else Left $ "unrecognised journal file path: " ++ f)
                       journalM
      strEs = [dateE, descE, acct1E, acct2E, journalE]
      amtEs = [amt1E, amt2E]
      errs = lefts strEs ++ lefts amtEs
      [date,desc,acct1,acct2,journalpath] = rights strEs
      [amt1,amt2] = rights amtEs
      -- if no errors so far, generate a transaction and balance it or get the error.
      tE | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (head $ lines e)]) Right
                        (balanceTransaction $ nulltransaction {
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
    setMessage $ string $ intercalate "; " errs
    redirect RedirectTemporary RegisterR

   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    liftIO $ appendToJournalFile journalpath $ showTransaction t'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
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
  let textE = maybe (Left "No value provided") Right textM
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> if f `elem` journalFilePaths j
                              then Right f
                              else Left "unrecognised journal file path")
                       journalM
      strEs = [textE, journalE]
      errs = lefts strEs
      [text,journalpath] = rights strEs
  -- display errors or perform edit
  if not $ null errs
   then do
    setMessage $ string $ intercalate "; " errs
    redirect RedirectTemporary JournalR

   else do
    -- try to avoid unnecessary backups or saving invalid data
    filechanged' <- liftIO $ journalSpecifiedFileIsNewer j journalpath
    told <- liftIO $ readFileStrictly journalpath
    let tnew = filter (/= '\r') text
        changed = tnew /= told || filechanged'
    if not changed
     then do
       setMessage $ string $ "No change"
       redirect RedirectTemporary JournalR
     else do
      jE <- liftIO $ journalFromPathAndString Nothing journalpath tnew
      either
       (\e -> do
          setMessage $ string e
          redirect RedirectTemporary JournalR)
       (const $ do
          liftIO $ writeFileWithBackup journalpath tnew
          setMessage $ string $ printf "Saved journal %s\n" (show journalpath)
          redirect RedirectTemporary JournalR)
       jE

-- | Handle an import page post.
postImportForm :: Handler RepPlain
postImportForm = do
  setMessage $ string $ "can't handle file upload yet"
  redirect RedirectTemporary JournalR
  -- -- get form input values, or basic validation errors. E means an Either value.
  -- fileM <- runFormPost' $ maybeFileInput "file"
  -- let fileE = maybe (Left "No file provided") Right fileM
  -- -- display errors or import transactions
  -- case fileE of
  --  Left errs -> do
  --   setMessage $ string errs
  --   redirect RedirectTemporary JournalR

  --  Right s -> do
  --    setMessage $ string $ s
  --    redirect RedirectTemporary JournalR

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
registerReportAsHtml _ td items = [$hamlet|
%table.registerreport
 %tr.headings
  ^headings^
 $forall number.items i
  ^itemAsHtml' i^
|]
 where
   number = zip [1..]
   headings = [$hamlet|
               %th.date!align=left Date
               %th.description!align=left Description
               %th.account!align=left Account
               %th.amount!align=right Amount
               %th.balance!align=right Balance
               |]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, RegisterReportItem) -> Hamlet AppRoute
   itemAsHtml TD{p=p} (n, (ds, posting, b)) = [$hamlet|
     %tr.item.$evenodd$.$firstposting$
      %td.date $date$
      %td.description $desc$
      %td.account
       %a!href=$aurl$ $acct$
      %td.amount!align=right $mixedAmountAsHtml.pamount.posting$
      %td.balance!align=right $mixedAmountAsHtml.b$
     |] where
       evenodd = if even n then "even" else "odd"
       (firstposting, date, desc) = case ds of Just (da, de) -> ("firstposting", show da, de)
                                               Nothing -> ("", "", "")
       acct = paccount posting
       aurl = printf ".?a=%s%s" (accountNameToAccountRegex acct) p' :: String
       p' = if null p then "" else printf "&p=%s" p

mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ show b
    where addclass = printf "<span class=\"%s\">%s</span>" c
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

----------------------------------------------------------------------

-- | A standalone journal edit form page.
-- getEditR :: Handler RepHtml
-- getEditR = do
--   (a, p, _, _, _, msg, here) <- getHandlerData
--   today <- liftIO getCurrentDay
--   -- reload journal's text without parsing, if changed     -- XXX are we doing this right ?
--   j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
--   changed <- liftIO $ journalFileIsNewer j
--   s <- liftIO $ if changed then readFile (filepath j) else return (jtext j) -- XXX readFile may throw an error
--   let td = mktd{here=here, title="hledger journal edit", msg=msg, a=a, p=p, j=j, today=today}
--   hamletToRepHtml $ pageLayout td $ editform td s

----------------------------------------------------------------------

-- | An add form with template files reloaded at runtime.
getAddformRTR :: Handler RepHtml
getAddformRTR = do
  (a, p, _, _, j, msg, here) <- getHandlerData
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger add transaction", msg=msg, a=a, p=p, j=j, today=today}
      descriptions = sort $ nub $ map tdescription $ jtxns j
      acctnames = sort $ journalAccountNamesUsed j
      postingData n = [
                       (["acctlabel"], hdstring acctlabel)
                      ,(["acctvar"],   hdstring acctvar)
                      ,(["acctnames"], hdstringlist acctnames)
                      ,(["amtfield"],  HDHtml $ renderHamletAsHtml amtfield)
                      ,(["accthelp"],  hdstring accthelp)
                      ,(["amthelp"],   hdstring amthelp)
                      ] :: HamletMap AppRoute
          where
            numbered = (++ show n)
            acctvar = numbered "account"
            amtvar = numbered "amount"
            (acctlabel, accthelp, amtfield, amthelp)
                | n == 1     = ("To account"
                              ,"eg: expenses:food"
                              ,[$hamlet|
                                %td!style=padding-left:1em;
                                 Amount:
                                %td
                                 %input.textinput!size=15!name=$amtvar$!value=""
                                |]
                              ,"eg: $6"
                              )
                | otherwise = ("From account"
                              ,"eg: assets:bank:checking"
                              ,nulltemplate
                              ,""
                              )
  pfields1 <- renderHamletFileRT "addformpostingfields.hamlet" (postingData 1)
  pfields2 <- renderHamletFileRT "addformpostingfields.hamlet" (postingData 2)
  addform  <- renderHamletFileRT "addform.hamlet" ([
                                                 (["date"], hdstring "today")
                                                ,(["desc"], hdstring "")
                                                ,(["descriptions"], hdstringlist descriptions)
                                                ,(["datehelp"], hdstring "eg: 2010/7/20")
                                                ,(["deschelp"], hdstring "eg: supermarket (optional)")
                                                ,(["postingfields1"], HDHtml pfields1)
                                                ,(["postingfields2"], HDHtml pfields2)
                                                ] :: HamletMap AppRoute)
  hamletToRepHtml $ pageLayout td $ htmlAsHamlet addform

