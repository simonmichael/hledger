-- | POST helpers.

module Handler.Post where

import Import

import Control.Applicative
import Data.Either (lefts,rights)
import Data.List (intercalate, sort)
import qualified Data.List as L (head) -- qualified keeps dev & prod builds warning-free
import Data.Maybe
import Data.Text (unpack)
import qualified Data.Text as T
import Text.Parsec (digit, eof, many1, string)
import Text.Printf (printf)

import Hledger.Utils
import Hledger.Data
import Hledger.Read
import Hledger.Cli


-- | Handle a post from any of the edit forms.
handlePost :: Handler Html
handlePost = do
  action <- lookupPostParam  "action"
  case action of Just "add"    -> handleAdd
                 Just "edit"   -> handleEdit
                 Just "import" -> handleImport
                 _             -> invalidArgs ["invalid action"]

-- | Handle a post from the transaction add form.
handleAdd :: Handler Html
handleAdd = do
  VD{..} <- getViewData
  -- XXX port to yesod-form later
  -- get form input values. M means a Maybe value.
  journalM <- lookupPostParam  "journal"
  dateM <- lookupPostParam  "date"
  descM <- lookupPostParam  "description"
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today . strip . unpack) dateM
      descE = Right $ maybe "" unpack descM
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left $ "unrecognised journal file path: " ++ f'
                              )
                       journalM
      estrs = [dateE, descE, journalE]
      (errs1, [date,desc,journalpath]) = (lefts estrs, rights estrs) -- XXX irrefutable

  (params,_) <- runRequestBody
  -- mtrace params
  let paramnamep s = do {string s; n <- many1 digit; eof; return (read n :: Int)}
      acctparams = sort
                   [ (n,v) | (k,v) <- params
                   , let en = parsewith (paramnamep "account") $ T.unpack k
                   , isRight en
                   , let Right n = en
                   ]
      amtparams =  sort
                   [ (n,v) | (k,v) <- params
                   , let en = parsewith (paramnamep "amount") $ T.unpack k
                   , isRight en
                   , let Right n = en
                   ]
      num = length acctparams
      paramErrs | not $ length amtparams `elem` [num, num-1] = ["different number of account and amount parameters"]
                | otherwise = catMaybes
                              [if map fst acctparams == [1..num] then Nothing else Just "misnumbered account parameters"
                              ,if map fst amtparams == [1..num] || map fst amtparams == [1..(num-1)] then Nothing else Just "misnumbered amount parameters"
                              ]
      eaccts = map (parsewith (accountnamep <* eof) . strip . T.unpack . snd) acctparams
      eamts  = map (parseWithCtx nullctx (amountp <* eof) . strip . T.unpack . snd) amtparams
      (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
      (amts', amtErrs) = (rights eamts, map show $ lefts eamts)
      amts | length amts' == num = amts'
           | otherwise           = amts' ++ [missingamt]

      -- if no errors so far, generate a transaction and balance it or get the error.
      errs = errs1 ++ if null paramErrs then (acctErrs ++ amtErrs) else paramErrs
      et | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (L.head $ lines e)]) Right
                        (balanceTransaction Nothing $ nulltransaction {
                            tdate=parsedate date
                           ,tdescription=desc
                           ,tpostings=[nullposting{paccount=acct, pamount=mixed amt} | (acct,amt) <- zip accts amts]
                           })

  -- display errors or add transaction
  case et of
   Left errs' -> do
    error $ show errs' -- XXX
    -- save current form values in session
    -- setMessage $ toHtml $ intercalate "; " errs
    setMessage [shamlet|
                 Errors:<br>
                 $forall e<-errs'
                  \#{e}<br>
               |]
   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    liftIO $ do ensureJournalFileExists journalpath
                appendToJournalFileOrStdout journalpath $ showTransaction t'
    setMessage [shamlet|<span>Transaction added.|]

  redirect (JournalR) -- , [("add","1")])

-- personForm :: Html -> MForm Handler (FormResult Person, Widget)
-- personForm extra = do
--     (nameRes, nameView) <- mreq textField "this is not used" Nothing
--     (ageRes, ageView) <- mreq intField "neither is this" Nothing
--     let personRes = Person <$> nameRes <*> ageRes
--     let widget = do
--             toWidget
--                 [lucius|
--                     ##{fvId ageView} {
--                         width: 3em;
--                     }
--                 |]
--             [whamlet|
--                 #{extra}
--                 <p>
--                     Hello, my name is #
--                     ^{fvInput nameView}
--                     \ and I am #
--                     ^{fvInput ageView}
--                     \ years old. #
--                     <input type=submit value="Introduce myself">
--             |]
--     return (personRes, widget)
--
--     ((res, widget), enctype) <- runFormGet personForm
--     defaultLayout
--         [whamlet|
--             <p>Result: #{show res}
--             <form enctype=#{enctype}>
--                 ^{widget}
--         |]

-- | Handle a post from the journal edit form.
handleEdit :: Handler Html
handleEdit = do
  VD{..} <- getViewData
  -- get form input values, or validation errors.
  -- getRequest >>= liftIO (reqRequestBody req) >>= mtrace
  textM <- lookupPostParam "text"
  journalM <- lookupPostParam "journal"
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
    redirect JournalR

   else do
    -- try to avoid unnecessary backups or saving invalid data
    filechanged' <- liftIO $ journalSpecifiedFileIsNewer j journalpath
    told <- liftIO $ readFileStrictly journalpath
    let tnew = filter (/= '\r') text
        changed = tnew /= told || filechanged'
    if not changed
     then do
       setMessage "No change"
       redirect JournalR
     else do
      jE <- liftIO $ readJournal Nothing Nothing True (Just journalpath) tnew
      either
       (\e -> do
          setMessage $ toHtml e
          redirect JournalR)
       (const $ do
          liftIO $ writeFileWithBackup journalpath tnew
          setMessage $ toHtml (printf "Saved journal %s\n" (show journalpath) :: String)
          redirect JournalR)
       jE

-- | Handle a post from the journal import form.
handleImport :: Handler Html
handleImport = do
  setMessage "can't handle file upload yet"
  redirect JournalR
  -- -- get form input values, or basic validation errors. E means an Either value.
  -- fileM <- runFormPost $ maybeFileInput "file"
  -- let fileE = maybe (Left "No file provided") Right fileM
  -- -- display errors or import transactions
  -- case fileE of
  --  Left errs -> do
  --   setMessage errs
  --   redirect JournalR

  --  Right s -> do
  --    setMessage s
  --    redirect JournalR

