 Looking at this issue, the problem is that `hledger-ui --watch` has gradually increasing CPU and RAM usage over time. The issue mentions it's likely related to the file-watching library (fsnotify). Let me examine the hledger-ui code to find where file watching is implemented.

```diff
--- a/hledger-ui/Hledger/UI/Main.hs
+++ b/hledger-ui/Hledger/UI/Main.hs
@@ -1,3 +1,4 @@
+{-# LANGUAGE TupleSections #-}
 {-# LANGUAGE CPP #-}
 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE LambdaCase #-}
@@ -5,7 +6,6 @@
 {-# LANGUAGE RecordWildCards #-}
 {-# LANGUAGE ScopedTypeVariables #-}
 {-# LANGUAGE TemplateHaskell #-}
-{-# LANGUAGE TupleSections #-}
 
 -- | Main entry point for hledger-ui.
 
@@ -14,6 +14,7 @@
 import Control.Concurrent (threadDelay)
 import Control.Concurrent.Async (withAsync)
 import Control.Concurrent.STM
+import qualified Control.Exception as E
 import Control.Monad
 import Control.Monad.IO.Class (liftIO)
 import Data.Bifunctor (first)
@@ -21,6 +22,7 @@
 import Data.List
 import Data.Maybe
 import Data.Time
+import qualified Data.Set as S
 import Graphics.Vty (mkVty)
 import qualified Graphics.Vty as V
 import Lens.Micro.Platform
@@ -28,6 +30,7 @@
 import System.Directory
 import System.FilePath ((</>))
 import System.FSNotify
+import System.FSNotify.Devel (isDirectory, eventPath)
 import Text.Printf
 
 import Hledger
@@ -37,6 +40,7 @@
 import Hledger.UI.UIOptions
 import Hledger.UI.UITypes
 import Hledger.UI.UIUtils
+import System.IO (hPutStrLn, stderr)
 
 -- version :: String
 -- version = printf "1.32 hledger-ui-%s" (PACKAGE_VERSION :: String)
@@ -163,6 +167,7 @@ runBrickUi uopts0@UIOpts{..} = do
   -- 2. fork a thread which watches for changes to any files we read
   -- (the journal file, and included files), and if it sees one,
   -- reads the new data and signals the UI to reload and redraw.
+  -- To prevent memory leaks, we deduplicate watch events and debounce them.
 
   let
     -- The initial state, including a possibly empty list of initial errors.
@@ -178,6 +183,7 @@ runBrickUi uopts0@UIOpts{..} = do
     -- A TVar to communicate file changes from the file watcher to the UI.
     -- True means a change was detected, and the UI should reload.
     changeTVar :: TVar Bool <- newTVarIO False
+    lastEventTVar :: TVar (M.Map FilePath UTCTime) <- newTVarIO M.empty
 
   -- Start the file watcher thread.
   -- When a file change is detected, it sets the TVar to True.
@@ -194,7 +200,7 @@ runBrickUi uopts0@UIOpts{..} = do
           -- On macOS, use the polling watch manager to avoid the
           -- high CPU usage of the native fsevents-based one.
           -- https://github.com/simonmichael/hledger/issues/610
-          -- But this causes high CPU usage on linux (#1617).
+          -- But this causes high CPU usage on linux (#1617, #836).
           -- So we use the default (native) manager on linux and windows.
           -- conf = defaultConfig { confUsePolling = True }
           --
@@ -203,14 +209,55 @@ runBrickUi uopts0@UIOpts{..} = do
           -- and the native one on linux and windows.
           -- conf = defaultConfig { confUsePolling = True }
           --
-          -- 2021: try the default manager on all platforms.
+          -- 2021: try the default manager on all platforms. Still had issues.
+          --
+          -- 2024: Use the default manager, but add debouncing and deduplication
+          -- to prevent the memory/CPU leak from excessive event processing.
           conf = defaultConfig
-      withManagerConf conf $ \mgr -> do
-        let
-          -- fsnotify fires many events per file change, so we ignore duplicates
-          -- by using a TVar to track the last event time per file.
-          watchFile f = do
+      -- Debounce interval: minimum seconds between processing events for the same file
+      let debounceSeconds = 0.5 :: NominalDiffTime
+      -- Maximum age of entries in our lastEvent map before we garbage collect
+      let gcIntervalSeconds = 60 :: NominalDiffTime
+      withManagerConf conf $ \mgr -> E.bracket
+        (newTVarIO M.empty)
+        (const $ return ())
+        (\lastEventTVar' -> do
+          let
+            -- Clean up old entries from the lastEvent map to prevent unbounded growth.
+            -- We keep entries that are newer than gcIntervalSeconds.
+            gcOldEvents now = do
+              lastEvents <- readTVarIO lastEventTVar'
+              let recentEvents = M.filter (\t -> diffUTCTime now t < gcIntervalSeconds) lastEvents
+              atomically $ writeTVar lastEventTVar' recentEvents
+
+            -- Check if we should process this event (not a duplicate within debounce window).
+            -- Returns True if we should process, and updates the last event time.
+            shouldProcessEvent now fp = atomically $ do
+              lastEvents <- readTVar lastEventTVar'
+              case M.lookup fp lastEvents of
+                Just lastTime | diffUTCTime now lastTime < debounceSeconds -> return False
+                _ -> do
+                  writeTVar lastEventTVar' (M.insert fp now lastEvents)
+                  return True
+
+            -- fsnotify fires many events per file change, so we ignore duplicates
+            -- by using a TVar to track the last event time per file.
+            -- We also filter out events for directories and non-journal files.
+            watchFile f = do
+              -- Get the canonical path to avoid duplicate watches for the same file
+              -- via different paths (symlinks, relative vs absolute, etc.)
+              canonicalF <- canonicalizePath f `E.catch` \(_ :: E.IOException) -> return f
+              -- Check if this file is already being watched (via canonical path)
+              alreadyWatched <- atomically $ do
+                watched <- readTVar lastEventTVar'
+                return $ M.member canonicalF watched
+              when alreadyWatched $ do
+                hPutStrLn stderr $ "Warning: " ++ f ++ " is already being watched (as " ++ show canonical