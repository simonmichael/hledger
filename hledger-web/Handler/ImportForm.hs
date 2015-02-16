-- -- | Handle a post from the journal import form.
-- handleImport :: Handler Html
-- handleImport = do
--   setMessage "can't handle file upload yet"
--   redirect JournalR
--   -- -- get form input values, or basic validation errors. E means an Either value.
--   -- fileM <- runFormPost $ maybeFileInput "file"
--   -- let fileE = maybe (Left "No file provided") Right fileM
--   -- -- display errors or import transactions
--   -- case fileE of
--   --  Left errs -> do
--   --   setMessage errs
--   --   redirect JournalR

--   --  Right s -> do
--   --    setMessage s
--   --    redirect JournalR

