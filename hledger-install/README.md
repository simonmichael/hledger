# hledger-install

A lot of specialised knowledge is still needed to be sure of 
successfully building and installing hledger in all cases. For example:
running stack setup if you don't have a suitable version of ghc..
installing with your current global resolver to minimise rebuilding.. 
unless it's too old, in which case use the (appropriate) latest resolver.. 
always use ghc 8.0.2+ on osx sierra.. 
don't try to install hledger-ui on windows.. 
etc. 

This installer script tries to automate the installation of stack and then hledger, 
as reliably and quickly as possible, on POSIX systems. 
(Windows systems have binaries and don't need this.)
