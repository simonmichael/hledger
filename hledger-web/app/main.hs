import Prelude              (IO)

import qualified Hledger.Web.Main

main :: IO ()
main = Hledger.Web.Main.main

-- more standard yesod main, for reloading experiments

-- import Yesod.Default.Config (fromArgs)
-- import Yesod.Default.Main   (defaultMain)
-- import Settings             (parseExtra)
-- import Application          (makeApplication)

-- import Hledger
-- import Hledger.Web (defwebopts)

-- main = do
--   j <- defaultJournal
--   defaultMain (fromArgs parseExtra) (makeApplication defwebopts j)
