{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-|

Embedded help files (man pages).

|-}

module Hledger.Cli.DocFiles (

   docFiles
  ,docTopics
  ,lookupDocNroff
  ,lookupDocTxt

  ) where

import Prelude ()
import Prelude.Compat
import Data.FileEmbed
import Data.String

type Topic = String

-- XXX assumes cwd is the hledger package directory, for now ghci must be run from there
docFiles :: IsString a => [(Topic, (a, a))]
docFiles = [
   ("cli",
    ($(embedStringFile $ "../hledger/doc/hledger.1"),
     $(embedStringFile $ "../hledger/doc/hledger.1.txt")))
  ,("ui",
    ($(embedStringFile $ "../hledger-ui/doc/hledger-ui.1"),
     $(embedStringFile $ "../hledger-ui/doc/hledger-ui.1.txt")))
  ,("web",
    ($(embedStringFile $ "../hledger-web/doc/hledger-web.1"),
     $(embedStringFile $ "../hledger-web/doc/hledger-web.1.txt")))
  ,("api",
    ($(embedStringFile $ "../hledger-api/doc/hledger-api.1"),
     $(embedStringFile $ "../hledger-api/doc/hledger-api.1.txt")))
  ,("journal",
    ($(embedStringFile $ "../hledger-lib/doc/hledger_journal.5"),
     $(embedStringFile $ "../hledger-lib/doc/hledger_journal.5.txt")))
  ,("csv",
    ($(embedStringFile $ "../hledger-lib/doc/hledger_csv.5"),
     $(embedStringFile $ "../hledger-lib/doc/hledger_csv.5.txt")))
  ,("timeclock",
    ($(embedStringFile $ "../hledger-lib/doc/hledger_timeclock.5"),
     $(embedStringFile $ "../hledger-lib/doc/hledger_timeclock.5.txt")))
  ,("timedot",
    ($(embedStringFile $ "../hledger-lib/doc/hledger_timedot.5"),
     $(embedStringFile $ "../hledger-lib/doc/hledger_timedot.5.txt")))
  ]

docTopics :: [Topic]
docTopics = map fst docFiles

lookupDocNroff :: IsString a => Topic -> a
lookupDocNroff name =
  maybe (fromString $ "No such help topic: "++name) fst $ lookup name docFiles

lookupDocTxt :: IsString a => Topic -> a
lookupDocTxt name =
  maybe (fromString $ "No such help topic: "++name) snd $ lookup name docFiles

