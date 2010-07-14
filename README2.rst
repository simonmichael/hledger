---
title: hledger development
---
hledger development
===================

**Techie intro**

hledger_ is a remix, in haskell_, of John Wiegley's excellent ledger_ accounting tool.
It reads a plain text journal_ or timelog_ file describing your transactions
and displays reports via command line, curses or web interfaces.

The hledger project aims to produce:

- a practical, accessible, dependable tool for end users
- a useful library and toolbox for finance-minded haskell programmers
- a successful, time-and-money-solvent project within a thriving ecosystem of financial software projects.

hledger is free software by `Simon Michael`_ & `co.`_, released under GNU GPLv3.

**Learn**
 news_, manual_, screenshots_, `some extra tips`_

**Download**
 ``cabal install hledger``, 
 or try these ready-to-run binaries_,
 or see the `installing docs <MANUAL.html#installing>`_

**Develop**
 ``darcs get http://joyful.com/repos/hledger``, 
 `browse the repo`_, 
 `hackage page`_, 
 `hledger-lib api docs`_, 
 `hledger api docs`_, 
 `hledger-lib sourcegraph report`_, 
 `hledger sourcegraph report`_, 
 benchmark_\/profile_\/heap_\/coverage_ reports,
 `developer notes`_

.. raw:: html

 <a name="support" />

**Support**

- chat Simon (sm) on the `#ledger`_ irc channel which we share, or `email me`_
- report problems at `bugs.hledger.org <http://bugs.hledger.org>`_
- share and test journal snippets at paste . hledger.org
- .. raw:: html

     <form action="http://groups.google.com/group/hledger/boxsubscribe" >
       join the hledger mail list at <a href="http://list.hledger.org">list.hledger.org</a>. Your email:
       <input type=text name=email><input type=submit name="sub" value="Subscribe">
     </form>

**Related projects**

- John Wiegley's ledger_ inspired hledger, and we try to stay compatible. You can often use both tools on the same journal file.
- Uwe Hollerbach's umm_ is another haskell tool inspired by h/ledger.
- Tim Docker's ledger-reports_ uses hledger as a library to generate `html reports`_. 
- I have a few older bits and pieces `here <http://joyful.com/Ledger>`_.

.. raw:: html

 <a href="http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=shortlog"><img src=http://joyful.com/repos/hledger/commits.png border=0></a>
 <a href="https://www.google.com/analytics/reporting/?reset=1&id=15489822" accesskey="a"></a>


.. _hledger:              README.html
.. _journal:              http://joyful.com/repos/hledger/data/sample.journal
.. _timelog:              http://joyful.com/repos/hledger/data/sample.timelog
.. _command line:         SCREENSHOTS.html#hledger-screen-1
.. _curses:               SCREENSHOTS.html#sshot
.. _web interface:        http://demo.hledger.org
.. _mail list:            http://list.hledger.org
.. _issue tracker:        http://bugs.hledger.org
.. _binaries:             http://hledger.org/binaries/
.. _manual:               MANUAL.html
.. _news:                 NEWS.html
.. _screenshots:          SCREENSHOTS.html
.. _hledger-lib api docs: http://joyful.com/repos/hledger/hledger-lib/dist/doc/html/hledger-lib/index.html
.. _hledger api docs:     http://joyful.com/repos/hledger/dist/doc/html/hledger/index.html
.. _hledger-lib sourcegraph report: http://joyful.com/repos/hledger/hledger-lib/SourceGraph/hledger-lib.html
.. _hledger sourcegraph report: http://joyful.com/repos/hledger/SourceGraph/hledger.html
.. _developer notes:      http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=plainblob;f=/NOTES
.. _benchmark:            http://hledger.org/profs/latest.bench
.. _profile:              http://hledger.org/profs/latest.prof
.. _heap:                 http://hledger.org/profs/latest.ps
.. _coverage:             http://hledger.org/profs/coverage/hpc_index_fun.html
.. _browse the repo:      http://joyful.com/darcsweb/darcsweb.cgi?r=hledger
.. _email me:             mailto:simon@joyful.com
.. _Simon Michael:        http://joyful.com
.. _co.:                  http://hledger.org/CONTRIBUTORS.html
.. _hackage page:         http://hackage.haskell.org/package/hledger
.. _#ledger:              irc://irc.freenode.net/#ledger
.. _haskell:              http://haskell.org
.. _ledger:               http://wiki.github.com/jwiegley/ledger
.. _umm:                  http://www.korgwal.com/umm/
.. _ledger-reports:       http://dockerz.net/repos/ledger-reports
.. _html reports:         http://dockerz.net/software/hledger_report_sample/report.html
.. _some extra tips:      http://podcastle.org/2009/10/09/pc-miniature-38-accounting-for-dragons/
