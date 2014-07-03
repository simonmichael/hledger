-- | POST helpers.

module Handler.Post where

import Import

import Data.Either (lefts,rights)
import Data.List (intercalate)
import qualified Data.List as L (head) -- qualified keeps dev & prod builds warning-free
import Data.Text (unpack)
import qualified Data.Text as T (null)
import Text.Printf (printf)

import Handler.Utils
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
  -- get form input values. M means a Maybe value.
  dateM <- lookupPostParam  "date"
  descM <- lookupPostParam  "description"
  acct1M <- lookupPostParam  "account1"
  amt1M <- lookupPostParam  "amount1"
  acct2M <- lookupPostParam  "account2"
  amt2M <- lookupPostParam  "amount2"
  journalM <- lookupPostParam  "journal"
  -- supply defaults and parse date and amounts, or get errors.
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today . unpack) dateM
      descE = Right $ maybe "" unpack descM
      maybeNonNull = maybe Nothing (\t -> if T.null t then Nothing else Just t)
      acct1E = maybe (Left "to account required") (Right . unpack) $ maybeNonNull acct1M
      acct2E = maybe (Left "from account required") (Right . unpack) $ maybeNonNull acct2M
      amt1E = maybe (Left "amount required") (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx amountp . unpack) amt1M
      amt2E = maybe (Right missingamt)       (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx amountp . unpack) amt2M
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
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (L.head $ lines e)]) Right
                        (balanceTransaction Nothing $ nulltransaction { -- imprecise balancing
                           tdate=parsedate date
                          ,tdescription=desc
                          ,tpostings=[
                            nullposting{paccount=acct1, pamount=mixed amt1}
                           ,nullposting{paccount=acct2, pamount=mixed amt2}
                           ]
                          })
  -- display errors or add transaction
  -- XXX currently it's still possible to write an invalid entry, eg by adding space space ; after the first account name
  case tE of
   Left errs' -> do
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
    -- setMessage $ toHtml $ (printf "Added transaction:\n%s" (show t') :: String)
    setMessage [shamlet|<span>Added transaction:<small><pre>#{chomp $ show t'}</pre></small>|]

  redirect (JournalR, [("add","1")])

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

