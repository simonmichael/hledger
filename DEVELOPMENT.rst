---
title: hledger development
---
hledger development
===================

**Support** for users and developers:

- chat Simon (sm) on the `#ledger`_ irc channel which we share, or `email me`_
- `report <http://code.google.com/p/hledger/issues/entry>`_ problems at `bugs.hledger.org <http://bugs.hledger.org>`_ (`view all <http://bugs.hledger.org/grid>`_)
- share and test journal snippets at paste . hledger.org
- .. raw:: html

     <form action="http://groups.google.com/group/hledger/boxsubscribe" >
       join the hledger mail list at <a href="http://list.hledger.org">list.hledger.org</a>. Your email:
       <input type=text name=email><input type=submit name="sub" value="Subscribe">
     </form>

**Goals:** the hledger project aims to produce

- a practical, accessible, dependable tool for end users
- a useful library and toolbox for finance-minded haskell programmers
- a successful, time-and-money-solvent project within a thriving ecosystem of financial software projects.

**Code:**

 :: 

  darcs get --lazy http://joyful.com/repos/hledger
  cd hledger
  make or make install

 `Release notes`_, 
 `browse the repo`_, 
 `hackage page`_, 
 `combined api docs`_, 
 benchmark_\/profile_\/heap_\/`coverage reports`_,
 `developer notes`_

.. `hledger-lib sourcegraph report`_, 
.. `hledger sourcegraph report`_, 

.. raw:: html

 <a href="http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=shortlog"><img src=http://joyful.com/repos/hledger/commits.png border=0></a>
 <a href="https://www.google.com/analytics/reporting/?reset=1&id=15489822" accesskey="a"></a>

**Related projects**

- John Wiegley's ledger_ inspired hledger.
- h/ledger inspired Uwe Hollerbach's umm_.
- Tim Docker's ledger-reports_ builds on hledger to generate `html reports`_. 
- I have a few older bits and pieces `here <http://joyful.com/Ledger>`_.


.. _hledger:              README.html
.. _journal:              http://joyful.com/repos/hledger/data/sample.journal
.. _timelog:              http://joyful.com/repos/hledger/data/sample.timelog
.. _command line:         SCREENSHOTS.html#hledger-screen-1
.. _curses:               SCREENSHOTS.html#sshot
.. _web interface:        http://demo.hledger.org
.. _mail list:            http://list.hledger.org
.. _issue tracker:        http://bugs.hledger.org
.. _Release notes:        NEWS.html
.. _combined api docs:      http://hledger.org/api-doc/
.. _hledger-lib sourcegraph report: http://joyful.com/repos/hledger/hledger-lib/SourceGraph/hledger-lib.html
.. _hledger sourcegraph report: http://joyful.com/repos/hledger/SourceGraph/hledger.html
.. _developer notes:      http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=plainblob;f=/NOTES
.. _benchmark:            http://hledger.org/profs/latest.bench
.. _profile:              http://hledger.org/profs/latest.prof
.. _heap:                 http://hledger.org/profs/latest.ps
.. _coverage reports:     http://hledger.org/profs/coverage/hpc_index_fun.html
.. _browse the repo:      http://joyful.com/darcsweb/darcsweb.cgi?r=hledger
.. _email me:             mailto:simon@joyful.com?subject=hledger:
.. _Simon Michael:        http://joyful.com
.. _co.:                  http://hledger.org/CONTRIBUTORS.html
.. _hackage page:         http://hackage.haskell.org/package/hledger
.. _#ledger:              irc://irc.freenode.net/#ledger
.. _haskell:              http://haskell.org
.. _ledger:               http://wiki.github.com/jwiegley/ledger
.. _umm:                  http://www.korgwal.com/umm/
.. _ledger-reports:       http://dockerz.net/repos/ledger-reports
.. _html reports:         http://dockerz.net/software/hledger_report_sample/report.html
