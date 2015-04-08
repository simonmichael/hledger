# some standard operations in a cabal sandbox, optionally with specified GHC version

#include help-system.mks

sandbox.mk-default: help


$(call def-help-subsection,-- Default sandbox)

sandbox-setup:\
	$(call def-help,sandbox-setup, set up a cabal sandbox for hledger using the default GHC version)
	@make sandbox-init sandbox-add-sources

sandbox-init:
	cabal sandbox init

sandbox-add-sources:
	cabal sandbox add-source hledger-lib hledger hledger-web

sandbox-list-sources:\
	$(call def-help,sandbox-list-sources,)
	cabal sandbox list-sources

sandbox-pkg-list:\
	$(call def-help,sandbox-pkg-list,\
	)
	cabal sandbox hc-pkg list

sandbox-pkg-check:\
	$(call def-help,sandbox-pkg-check,)
	cabal sandbox hc-pkg check

sandbox-install-dry-lib:\
	$(call def-help,sandbox-install-dry-lib,)
	cabal install --dry ./hledger-lib

sandbox-install-dry-cli:\
	$(call def-help,sandbox-install-dry-cli,)
	cabal install --dry ./hledger

sandbox-install-dry-web:\
	$(call def-help,sandbox-install-dry-web,)
	cabal install --dry ./hledger-web

sandbox-install-dep-lib:\
	$(call def-help,sandbox-install-dep-lib,)
	cabal install --only-dep ./hledger-lib

sandbox-install-dep-cli:\
	$(call def-help,sandbox-install-dep-cli,)
	cabal install --only-dep ./hledger

sandbox-install-dep-web:\
	$(call def-help,sandbox-install-dep-web,)
	cabal install --only-dep ./hledger-web

sandbox-install-lib:\
	$(call def-help,sandbox-install-lib, install hledger-lib)
	cabal install ./hledger-lib

sandbox-install-cli:\
	$(call def-help,sandbox-install-cli, install hledger-lib and hledger)
	cabal install ./hledger

sandbox-install-web:\
	$(call def-help,sandbox-install-web, install hledger-lib and hledger and hledger-web)
	cabal install ./hledger-web

sandbox-repl-lib:\
	$(call def-help,sandbox-repl-lib, start a cabal REPL for the hledger-lib package)
	(cd hledger-lib; cabal --sandbox-config-file=../cabal.sandbox.config repl)

sandbox-repl-cli:\
	$(call def-help,sandbox-repl-cli, start a cabal REPL for the hledger package)
	(cd hledger; cabal --sandbox-config-file=../cabal.sandbox.config repl exe:hledger)

sandbox-repl-web:\
	$(call def-help,sandbox-repl-web, start a cabal REPL for the hledger-web package)
	(cd hledger-web; cabal --sandbox-config-file=../cabal.sandbox.config repl exe:hledger-web)



$(call def-help-subsection,-- Sandbox for specified GHC version (eg: make sandbox-7.8.4-setup))

 # .cabal-sandbox.ghc%:
sandbox-%-setup: \
	$(call def-help,sandbox-%-setup, set up a cabal sandbox for hledger using the specified GHC version)
	@make sandbox-$*-init sandbox-$*-add-sources

sandbox-%-init:
	cabal --sandbox-config-file=cabal.sandbox.$*.config sandbox init --sandbox .cabal-sandbox.$*
	@echo "now manually fix the ghc version in package-db: in cabal.sandbox.$*.config"

sandbox-%-add-sources:
	cabal --sandbox-config-file=cabal.sandbox.$*.config sandbox add-source --sandbox .cabal-sandbox.$* hledger-lib hledger hledger-web

sandbox-%-list-sources:\
	$(call def-help,sandbox-%-list-sources,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config sandbox list-sources --sandbox .cabal-sandbox.$*

sandbox-%-pkg-list: \
	$(call def-help,sandbox-%-pkg-list,\
	)
	ghc-pkg-$* --package-db .cabal-sandbox.$*/*$*-packages.conf.d list
#	ghc-pkg-$* --global-package-db /usr/local/lib/ghc-$*/package.conf.d --package-db .cabal-sandbox.$*/*$*-packages.conf.d list

sandbox-%-pkg-check:\
	$(call def-help,sandbox-%-pkg-check,)
	ghc-pkg-$* --package-db .cabal-sandbox.$*/*$*-packages.conf.d check
#	ghc-pkg-$* --global-package-db /usr/local/lib/ghc-$*/package.conf.d --package-db .cabal-sandbox.$*/*$*-packages.conf.d check

sandbox-%-install-dry-lib:\
	$(call def-help,sandbox-%-install-dry-lib,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --dry ./hledger-lib

sandbox-%-install-dry-cli:\
	$(call def-help,sandbox-%-install-dry-cli,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --dry ./hledger

sandbox-%-install-dry-web:\
	$(call def-help,sandbox-%-install-dry-web,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --dry ./hledger-web

sandbox-%-install-dep-lib:\
	$(call def-help,sandbox-%-install-dep-lib,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --only-dep ./hledger-lib

sandbox-%-install-dep-cli:\
	$(call def-help,sandbox-%-install-dep-cli,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --only-dep ./hledger

sandbox-%-install-dep-web:\
	$(call def-help,sandbox-%-install-dep-web,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* --only-dep ./hledger-web

sandbox-%-install-lib:\
	$(call def-help,sandbox-%-install-lib,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* ./hledger-lib

sandbox-%-install-cli:\
	$(call def-help,sandbox-%-install-cli,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* ./hledger

sandbox-%-install-web:\
	$(call def-help,sandbox-%-install-web,)
	cabal --sandbox-config-file=cabal.sandbox.$*.config install -w ghc-$* ./hledger-web

sandbox-%-repl-lib:\
	$(call def-help,sandbox-%-repl-lib,)
	(cd hledger-lib; cabal --sandbox-config-file=../cabal.sandbox.$*.config repl)

sandbox-%-repl-cli:\
	$(call def-help,sandbox-%-repl-cli,)
	(cd hledger; cabal --sandbox-config-file=../cabal.sandbox.$*.config repl exe:hledger)

sandbox-%-repl-web:\
	$(call def-help,sandbox-%-repl-web,)
	(cd hledger-web; cabal --sandbox-config-file=../cabal.sandbox.$*.config repl exe:hledger-web)


Clean-sandboxes:\
	$(call def-help,Clean-sandboxes, delete all sandboxes in this directory)
	rm -rf .cabal-sandbox* cabal.sandbox*
