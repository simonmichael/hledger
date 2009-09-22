{-# LANGUAGE CPP #-}
{-| 

The Commands package defines all the commands offered by the hledger
application, like \"register\" and \"balance\".  This module exports all
the commands; you can also import individual modules if you prefer.

-}

module Commands.All (
                     module Commands.Add,
                     module Commands.Balance,
                     module Commands.Convert,
                     module Commands.Histogram,
                     module Commands.Print,
                     module Commands.Register,
                     module Commands.Stats,
#ifdef VTY
                     module Commands.UI,
#endif
#ifdef WEB
                     module Commands.Web,
#endif
              )
where
import Commands.Add
import Commands.Balance
import Commands.Convert
import Commands.Histogram
import Commands.Print
import Commands.Register
import Commands.Stats
#ifdef VTY
import Commands.UI
#endif
#ifdef WEB
import Commands.Web
#endif
