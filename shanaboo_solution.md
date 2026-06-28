 ```diff
--- a/hledger-ui/Hledger/UI/Main.hs
+++ b/hledger-ui/Hledger/UI/Main.hs
@@ -1,3 +1,4 @@
+{-# LANGUAGE CPP #-}
 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE RecordWildCards #-}
 {-# LANGUAGE ScopedTypeVariables #-}
@@ -5,6 +6,7 @@
 {-# LANGUAGE TypeFamilies #-}
 {-# LANGUAGE ViewPatterns #-}
 {-# LANGUAGE LambdaCase #-}
+{-# LANGUAGE TupleSections #-}
 
 {-|
 
@@ -15,6 +17,7 @@
 
 module Hledger.UI.Main where
 
+import Control.Concurrent (threadDelay)
 import Control.Concurrent.Async (withAsync, waitCatch)
 import Control.Concurrent.STM
 import Control.Exception (bracket_)
@@ -24,7 +27,7 @@ import Control.Monad (when, void)
 import Data.Bits ((.|.))
 import Data.List
 import Data.Maybe
-import Data.Time (getCurrentTime, utctDay, Day)
+import Data.Time (getCurrentTime, utctDay, Day, diffUTCTime)
 import Data.FileEmbed (makeRelativeToProject, embedFile)
 import qualified Data.Text as T
 import Graphics.Vty (mkVty, Mode(..))
@@ -33,7 +36,7 @@ import System.Directory
 import System.FilePath
 import System.FSNotify
   (Event(..), watchDir, withManager, WatchManager, StopWatching, EventIsDirectory(..))
-import System.FSNotify.Devel (isDirectory, fp)
+import System.FSNotify.Devel (isDirectory)
 import Brick
 import Brick.BChan
 import qualified Brick.Widgets.Edit as E
@@ -53,6 +56,7 @@ import Hledger.UI.UIOptions
 import Hledger.UI.UITypes
 import Hledger.UI.UIUtils
 import Hledger.UI.Editor
+import Hledger.UI.Watch (withWatchManager, watchFileOrDirectory)
 
 -- | Version string with program name.
 prognameandversion :: String
@@ -195,7 +199,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
       -- start a thread to watch for changes to the journal file(s)
       -- and send events to the app
       withAsync
-        (withManager $ \mgr -> do
+        (withWatchManager $ \mgr -> do
           -- we have two kinds of file change events to handle:
           -- 1. journal file(s) specified by -f or LEDGER_FILE or default,
           --    which should reload the journal
@@ -209,7 +213,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           --    (hledger doesn't use them, but they might be present)
           --    and we don't need to watch the whole tree.
           let
-            watch f isignored = watchDir mgr (takeDirectory f) (const True) $ \e -> do
+            watch f isignored = watchFileOrDirectory mgr f $ \e -> do
               let eventfile = eventPath e
               -- when the file is not ignored, and is a journal file or directory, and is the one we're watching
               when (not (isignored eventfile) && (isJournalFile eventfile || isDirectory e) && eventfile == f) $ do
@@ -222,7 +226,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           -- watch the specified journal file, or the default journal file if none specified
           case mjournalfile of
             Just f  -> watch f isIgnoreFile
-            Nothing -> watch (journalFilePath def) (const True)  -- with no -f, watch default journal file and all included files
+            Nothing -> watch (fromMaybe (journalFilePath def) mjournalfile) (const True)  -- with no -f, watch default journal file and all included files
 
           -- Also watch the specified journal file's directory for new files.
           -- This allows the user to create a new file in the same directory
@@ -231,7 +235,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           -- XXX This is a bit of a hack. We should probably watch all included
           -- files' directories, but this is good enough for now.
           let journaldir = maybe (takeDirectory $ journalFilePath def) takeDirectory mjournalfile
-          watchDir mgr journaldir (const True) $ \e -> do
+          watchFileOrDirectory mgr journaldir $ \e -> do
             let eventfile = eventPath e
             when (not (isIgnoreFile eventfile) && isJournalFile eventfile) $ do
               -- debug1 "fsnotify" $ eventfile ++ " modified, reloading"
@@ -240,7 +244,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           -- keep this thread alive to keep the watchers alive
           forever $ threadDelay 1000000
         )
-        (\a -> do
+        (\a -> do
           -- run brick
           let
             buildVty = do
@@ -256,7 +260,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           -- wait for the file watcher thread to finish (it shouldn't)
           e <- waitCatch a
           whenLeft e $ \err -> writeChan (fatalErrorChan chan) (show err)
-        )
+        )
 
     -- without --watch, just run brick
     else do
@@ -316,7 +320,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
       -- start a thread to watch for changes to the journal file(s)
       -- and send events to the app
       withAsync
-        (withManager $ \mgr -> do
+        (withWatchManager $ \mgr -> do
           -- we have two kinds of file change events to handle:
           -- 1. journal file(s) specified by -f or LEDGER_FILE or default,
           --    which should reload the journal
@@ -330,7 +334,7 @@ runBrickApp uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts,reportspec_=rspe
           --    (hledger doesn't use them, but they might be present)
           --    and we don't need to watch the whole tree.
           let
-            watch f isignored = watchDir mgr (takeDirectory f) (const True