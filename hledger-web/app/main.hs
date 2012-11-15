import Prelude              (IO)
-- import Yesod.Default.Config (fromArgs)
-- import Yesod.Default.Main   (defaultMain)
-- import Settings             (parseExtra)
-- import Application          (makeApplication)

import qualified Hledger.Web.Main

main :: IO ()
-- main = defaultMain (fromArgs parseExtra) makeApplication
main = Hledger.Web.Main.main
