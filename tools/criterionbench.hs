{- Criterion-based benchmarks. Criterion displays and minimises the impact
of time variance and charts the results. -}

import Criterion.Main
import System.Environment (withArgs)
import qualified HledgerMain

main = defaultMain [
        bench "balance_100x100x10" $ nfIO $ withArgs ["balance", "-f", "100x100x10.ledger", ">/dev/null"] HledgerMain.main
       ]
