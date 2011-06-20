# hledger project makefile

# ghc 6.12 executables need a locale
export LANG=en_US.UTF-8

# command line to run during "make prof" and "make heap"
PROFCMD=bin/hledgerp -f data/1000x1000x10.journal balance >/dev/null

# command to run during "make coverage"
COVCMD=test

# executables to run during "make simplebench". They should be on the path
# or in the current directory. hledger executables for benchmarking should
# generally be the standard optimised cabal build, constrained to parsec 2.
BENCHEXES=hledger-0.12.1 hledger-0.13 hledger-0.14-ghc6.12.3 ledger
#BENCHEXES=hledger

# searchpath executable used for automatic recompilation, http://joyful.com/repos/searchpath
AUTOBUILD=sp --no-exts --no-default-map ghc --make

# misc. tools
BROWSE=google-chrome
BROWSE=open -a 'Google Chrome'
VIEWHTML=$(BROWSE)
VIEWPS=$(BROWSE)
VIEWPDF=$(BROWSE)
PRINT=lpr

PACKAGES=\
	hledger-lib \
	hledger \
	hledger-web \
	hledger-vty \
	hledger-chart
INCLUDEPATHS=\
	-ihledger-lib \
	-ihledger \
	-ihledger-web \
	-ihledger-vty \
	-ihledger-chart
MAIN=hledger/hledger-cli.hs

# all source files in the project (plus a few strays like Setup.hs & hlint.hs)
SOURCEFILES:= \
	hledger/*hs \
	hledger/Hledger/*hs \
	hledger/Hledger/*/*hs \
	hledger-*/*hs \
	hledger-*/Hledger/*hs \
	hledger-*/Hledger/*/*hs

# a more careful list suitable for for haddock
HADDOCKSOURCEFILES:= \
	hledger-lib/*hs \
	hledger-lib/Hledger/*hs \
	hledger-lib/Hledger/*/*hs \
	hledger/Hledger/*hs \
	hledger/Hledger/*/*hs \
	hledger-web/Hledger/*hs \
	hledger-web/Hledger/*/*hs \
	hledger-vty/Hledger/*hs \
	hledger-vty/Hledger/*/*hs
#	hledger-chart/Hledger/*hs
#	hledger-chart/Hledger/*/*hs

VERSIONHS=hledger/Hledger/Cli/Version.hs

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal

WEBFILES:= \
	hledger-web/.hledger/web/static/*.js \
	hledger-web/.hledger/web/static/*.css
#	hledger-web/.hledger/web/templates/* \

# DOCFILES:=README DOWNLOAD MANUAL DEVELOPMENT NEWS SCREENSHOTS CONTRIBUTORS
PATCHLEVEL:=$(shell expr `darcs changes --count --from-tag=\\\\\.` - 1)
WARNINGS:=-W -fwarn-tabs #-fwarn-orphans -fwarn-simple-patterns -fwarn-monomorphism-restriction -fwarn-name-shadowing
DEFINEFLAGS:=
PREFERMACUSRLIBFLAGS=-L/usr/lib
BUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) -DPATCHLEVEL=$(PATCHLEVEL)
LINUXRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) -O2 -static -optl-static -optl-pthread
MACRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) -O2 # -optl-L/usr/lib
#WINDOWSRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS)
TIME:=$(shell date +"%Y%m%d%H%M")

# file defining the current release version
VERSIONFILE=VERSION
# two or three-part version string, whatever's in VERSION
VERSION:=$(shell grep -v '^--' $(VERSIONFILE))
# three-part version string, 0-padded if necessary
ifeq ($(shell ghc -e "length (filter (=='.') \"$(VERSION)\")"), 1)
VERSION3:=$(VERSION).0
else
VERSION3:=$(VERSION)
endif
# files which should be updated when the version changes
VERSIONSENSITIVEFILES=\
	$(VERSIONHS) \
	MANUAL.markdown \
	DOWNLOAD.markdown \
	$(CABALFILES) \
	hledger-web/.hledger/web/.version \

#BINARYFILENAME=$(shell touch $(VERSIONHS); runhaskell -ihledger $(MAIN) --binary-filename)
RELEASEBINARYSUFFIX:=$(shell echo "-$(VERSION)-`uname`-`arch`" | tr '[:upper:]' '[:lower:]')

default: tag hledger

######################################################################
# BUILDING

# set version numbers, fetch dependencies, build and install standard binaries
# and libs from all hledger packages. A good thing to run first; the other
# allcabal rules require hledger-VERSION and hledger-lib-VERSION installed.
# You may want to change the version number in VERSION file first.
install: allcabalinstall

# set version numbers and configure all hledger packages
configure: allcabalconfigure

# set version numbers and build all hledger packages
build: allcabalbuild

# set version numbers and cabal test all hledger packages
cabaltest: allcabaltest

# run a cabal command in all hledger package dirs
allcabal%:
	for p in $(PACKAGES); do (echo doing cabal $* in $$p; cd $$p; cabal $*; echo); done

# run a command in all hledger package dirs
all%:
	for p in $(PACKAGES); do (echo doing $* in $$p; cd $$p; $*); done

# auto-recompile and run (something, eg unit tests) whenever a module changes.
autotest: sp
	rm -f bin/hledger
	$(AUTOBUILD) $(MAIN) -o bin/hledger -ihledger $(BUILDFLAGS) --run test

# as above for add-on programs
autoweb: sp linkhledgerwebdir
	rm -f bin/hledger-web
	$(AUTOBUILD) hledger-web/hledger-web.hs -o bin/hledger-web -ihledger-web -ihledger $(BUILDFLAGS) --run #-f test.journal

autovty: sp
	rm -f bin/hledger-vty
	$(AUTOBUILD) hledger-vty/hledger-vty.hs -o bin/hledger-vty -ihledger-vty -ihledger $(BUILDFLAGS) --run --help

autochart: sp
	rm -f bin/hledger-chart
	$(AUTOBUILD) hledger-chart/hledger-chart.hs -o bin/hledger-chart -ihledger-chart -ihledger $(BUILDFLAGS) --run --help

# check for sp and explain how to get it if not found.
sp:
	@/usr/bin/env which sp >/dev/null || \
	  (echo '"sp" is required for auto-compilation. darcs get http://joyful.com/repos/searchpath, make it and add it to your PATH'; exit 1)

# make symlinks so that running hledger-web from the top directory will
# use the in-development hledger-web support files. Cf Hledger.Web.Settings:
HLEDGERDATADIR:=.hledger
linkhledgerwebdir:
	mkdir -p $(HLEDGERDATADIR); ln -sf ../hledger-web/$(HLEDGERDATADIR)/web $(HLEDGERDATADIR)/web

# build the standalone unit test runner. Requires test-framework, which
# may not work on windows.
tools/unittest: tools/unittest.hs
	ghc --make -threaded -O2 tools/unittest.hs

# build the doctest runner
tools/doctest: tools/doctest.hs
	ghc --make tools/doctest.hs

# build the simple benchmark runner. Requires tabular.
tools/simplebench: tools/simplebench.hs
	ghc --make tools/simplebench.hs

# build the criterion-based benchmark runner. Requires criterion.
tools/criterionbench: tools/criterionbench.hs
	ghc --make tools/criterionbench.hs

# build the progression-based benchmark runner. Requires progression.
tools/progressionbench: tools/progressionbench.hs
	ghc --make tools/progressionbench.hs

# build the generatejournal tool
tools/generatejournal: tools/generatejournal.hs
	ghc --make tools/generatejournal.hs

######################################################################
# TESTING


######################################################################
# DOCUMENTATION


######################################################################
# RELEASING


######################################################################
# MISCELLANEOUS



######################################################################
# OLD PRE PKG SPLIT
######################################################################

######################################################################
# BUILDING

hledgerall: bin/hledger hledger-web hledger-vty hledger-chart

# force a compile even if binary exists, since we don't specify dependencies
.PHONY: bin/hledger hledger-web hledger-vty hledger-chart

# build developer binaries, as quickly as possible
# this one is named bin/ to avoid case clash on mac
bin/hledger:
	ghc --make $(MAIN) -o bin/hledger $(BUILDFLAGS)

bin/hledger-web:
	ghc --make hledger-web/hledger-web.hs -o bin/hledger-web -ihledger-web -ihledger $(BUILDFLAGS)

bin/hledger-vty:
	ghc --make hledger-vty/hledger-vty.hs -o bin/hledger-vty -ihledger-vty -ihledger $(BUILDFLAGS)

bin/hledger-chart:
	ghc --make hledger-chart/hledger-chart.hs -o bin/hledger-chart -ihledger-chart -ihledger $(BUILDFLAGS)

hledgernowarnings:
	ghc --make $(MAIN) -o bin/hledger $(BUILDFLAGS) -Werror -v0

# build the profiling-enabled binary. You may need to cabal install
# --reinstall -p some libs.
hledgerp:
	ghc --make $(MAIN) -prof -auto-all -o bin/hledgerp $(BUILDFLAGS)

# build the -fhpc hledger binary used for coverage reports and heap profiles.
# The associated .o files are kept separate from the regular ones.
hledgerhpc:
	ghc --make $(MAIN) -fhpc -o bin/hledgerhpc -outputdir .hledgerhpcobjs $(BUILDFLAGS)

# build the fastest binary we can
hledgeropt:
	ghc --make $(MAIN) -o bin/hledgeropt $(BUILDFLAGS) -O2 # -fvia-C # -fexcess-precision -optc-O3 -optc-ffast-math

# build portable releaseable binaries for gnu/linux
linuxbinaries: 	linuxbinary-hledger \
		linuxbinary-hledger-web \
		linuxbinary-hledger-vty \
		linuxbinary-hledger-chart
	@echo 'Please check the binaries look portable, then make compressbinaries:'
	-file bin/*`arch`

linuxbinary-%:
	ghc --make $*/$*.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

# XXX link errors
linuxbinary-hledger-chart:
	ghc --make hledger-chart/hledger-chart.hs -o bin/hledger-chart$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS) -lpixman-1 -v

# build a deployable binary for mac, using only standard osx libs
# The scary link commands are copied from ghc --make -v, with -framework GMP removed.
# The GMP framework is not installed on most macs, and linking without it doesn't break anything that I'm aware of.
hledgermac:
	ghc -c --make $(MAIN) $(MACRELEASEBUILDFLAGS) -o bin/$(BINARYFILENAME)
	gcc -o bin/$(BINARYFILENAME) -march=i686 -m32 hledger/hledger.o hledger/Hledger/Cli/Main.o hledger-lib/Hledger/Data.o hledger/Hledger/Cli/Commands.o hledger/Hledger/Cli/Options.o hledger/Hledger/Cli/Tests.o hledger/Hledger/Cli/Utils.o hledger/Hledger/Cli/Version.o hledger-lib/Hledger/Data/Utils.o hledger-lib/Hledger/Data/Account.o hledger-lib/Hledger/Data/AccountName.o hledger-lib/Hledger/Data/Amount.o hledger-lib/Hledger/Data/Commodity.o hledger-lib/Hledger/Data/Dates.o hledger-lib/Hledger/Data/Transaction.o hledger-lib/Hledger/Data/Journal.o hledger-lib/Hledger/Data/Ledger.o hledger-lib/Hledger/Data/Posting.o hledger-lib/Hledger/Data/TimeLog.o hledger-lib/Hledger/Data/Types.o hledger-lib/Hledger/Read.o hledger-lib/Hledger/Read/Utils.o hledger-lib/Hledger/Read/JournalReader.o hledger-lib/Hledger/Read/TimelogReader.o hledger/Hledger/Cli/Add.o hledger/Hledger/Cli/Balance.o hledger/Hledger/Cli/Convert.o hledger/Hledger/Cli/Histogram.o hledger/Hledger/Cli/Print.o hledger/Hledger/Cli/Register.o hledger/Hledger/Cli/Stats.o -L/usr/lib -L/Users/simon/.cabal/lib/time-1.2.0.3/ghc-6.12.3 -L/usr/lib -L/Users/simon/.cabal/lib/split-0.1.2/ghc-6.12.3 -L/opt/local/lib -L/Users/simon/.cabal/lib/safe-0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/regexpr-0.5.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/process-1.0.1.3 -L/Users/simon/.cabal/lib/mtlparse-0.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/haskeline-0.6.3.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/utf8-string-0.3.6/ghc-6.12.3 -L/Users/simon/.cabal/lib/terminfo-0.3.1.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtl-2.0.1.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/transformers-0.2.2.0/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/extensible-exceptions-0.1.1.1 -L/usr/local/lib/ghc-6.12.3/directory-1.0.1.1 -L/usr/local/lib/ghc-6.12.3/unix-2.4.0.2 -L/usr/local/lib/ghc-6.12.3/old-time-1.0.0.5 -L/usr/local/lib/ghc-6.12.3/old-locale-1.0.0.2 -L/Users/simon/.cabal/lib/csv-0.1.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/parsec-2.1.0.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/filepath-1.1.0.4 -L/usr/local/lib/ghc-6.12.3/containers-0.3.0.0 -L/usr/local/lib/ghc-6.12.3/array-0.3.0.1 -L/usr/local/lib/ghc-6.12.3/bytestring-0.9.1.7 -L/usr/local/lib/ghc-6.12.3/base-3.0.3.2 -L/usr/local/lib/ghc-6.12.3/syb-0.1.0.2 -L/Users/simon/.cabal/lib/HUnit-1.2.2.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/base-4.2.0.2 -L/usr/local/lib/ghc-6.12.3/integer-gmp-0.2.0.1 -L/usr/local/lib/ghc-6.12.3/ghc-prim-0.2.0.0 -L/usr/local/lib/ghc-6.12.3 -lHSrtsmain -lHStime-1.2.0.3 -lHSsplit-0.1.2 -lHSsafe-0.2 -lHSregexpr-0.5.1 -lHSprocess-1.0.1.3 -lHSmtlparse-0.0.1 -lHShaskeline-0.6.3.2 -liconv -lHSutf8-string-0.3.6 -lHSterminfo-0.3.1.3 -lncurses -lHSmtl-2.0.1.0 -lHStransformers-0.2.2.0 -lHSextensible-exceptions-0.1.1.1 -lHSdirectory-1.0.1.1 -lHSunix-2.4.0.2 -ldl -lHSold-time-1.0.0.5 -lHSold-locale-1.0.0.2 -lHScsv-0.1.1 -lHSparsec-2.1.0.1 -lHSfilepath-1.1.0.4 -lHScontainers-0.3.0.0 -lHSarray-0.3.0.1 -lHSbytestring-0.9.1.7 -lHSbase-3.0.3.2 -lHSsyb-0.1.0.2 -lHSHUnit-1.2.2.1 -lHSbase-4.2.0.2 -liconv -lHSinteger-gmp-0.2.0.1 -lHSghc-prim-0.2.0.0 -lHSrts -lm -ldl -u _ghczmprim_GHCziTypes_Izh_static_info -u _ghczmprim_GHCziTypes_Czh_static_info -u _ghczmprim_GHCziTypes_Fzh_static_info -u _ghczmprim_GHCziTypes_Dzh_static_info -u _base_GHCziPtr_Ptr_static_info -u _base_GHCziWord_Wzh_static_info -u _base_GHCziInt_I8zh_static_info -u _base_GHCziInt_I16zh_static_info -u _base_GHCziInt_I32zh_static_info -u _base_GHCziInt_I64zh_static_info -u _base_GHCziWord_W8zh_static_info -u _base_GHCziWord_W16zh_static_info -u _base_GHCziWord_W32zh_static_info -u _base_GHCziWord_W64zh_static_info -u _base_GHCziStable_StablePtr_static_info -u _ghczmprim_GHCziTypes_Izh_con_info -u _ghczmprim_GHCziTypes_Czh_con_info -u _ghczmprim_GHCziTypes_Fzh_con_info -u _ghczmprim_GHCziTypes_Dzh_con_info -u _base_GHCziPtr_Ptr_con_info -u _base_GHCziPtr_FunPtr_con_info -u _base_GHCziStable_StablePtr_con_info -u _ghczmprim_GHCziBool_False_closure -u _ghczmprim_GHCziBool_True_closure -u _base_GHCziPack_unpackCString_closure -u _base_GHCziIOziException_stackOverflow_closure -u _base_GHCziIOziException_heapOverflow_closure -u _base_ControlziExceptionziBase_nonTermination_closure -u _base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -u _base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -u _base_ControlziExceptionziBase_nestedAtomically_closure -u _base_GHCziWeak_runFinalizzerBatch_closure -u _base_GHCziTopHandler_runIO_closure -u _base_GHCziTopHandler_runNonIO_closure -u _base_GHCziConc_ensureIOManagerIsRunning_closure -u _base_GHCziConc_runSparks_closure -u _base_GHCziConc_runHandlers_closure -Wl,-search_paths_first -read_only_relocs warning -lHSffi
	-ghc -c --make hledger-web/hledger-web.hs $(MACRELEASEBUILDFLAGS) -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/'`
	-gcc -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/'` -march=i686 -m32 hledger-web/hledger-web.o hledger-web/Hledger/Web/Main.o hledger/Hledger/Cli/Options.o hledger/Hledger/Cli/Utils.o hledger/Hledger/Cli/Version.o hledger-lib/Hledger/Data.o hledger-web/Hledger/Web/App.o hledger-web/Hledger/Web/Files.o hledger-web/Hledger/Web/Settings.o hledger-lib/Hledger/Data/Account.o hledger-lib/Hledger/Data/AccountName.o hledger-lib/Hledger/Data/Amount.o hledger-lib/Hledger/Data/Commodity.o hledger-lib/Hledger/Data/Dates.o hledger-lib/Hledger/Data/Transaction.o hledger-lib/Hledger/Data/Journal.o hledger-lib/Hledger/Data/Ledger.o hledger-lib/Hledger/Data/Posting.o hledger-lib/Hledger/Data/TimeLog.o hledger-lib/Hledger/Data/Types.o hledger-lib/Hledger/Data/Utils.o hledger-lib/Hledger/Read.o hledger-lib/Hledger/Read/Utils.o hledger-lib/Hledger/Read/JournalReader.o hledger-lib/Hledger/Read/TimelogReader.o hledger/Hledger/Cli/Add.o hledger/Hledger/Cli/Balance.o hledger/Hledger/Cli/Print.o hledger/Hledger/Cli/Register.o -L/usr/lib -L/Users/simon/.cabal/lib/yesod-0.6.6/ghc-6.12.3 -L/usr/lib -L/opt/local/lib -L/Users/simon/.cabal/lib/data-default-0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/xss-sanitize-0.2.4/ghc-6.12.3 -L/Users/simon/.cabal/lib/web-routes-0.23.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/wai-extra-0.2.4.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/zlib-bindings-0.0.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/zlib-0.5.2.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/wai-0.2.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/tagsoup-0.11.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/split-0.1.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/safe-0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/regexpr-0.5.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/pureMD5-1.1.0.0/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/process-1.0.1.3 -L/Users/simon/.cabal/lib/persistent-0.3.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/web-routes-quasi-0.6.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/stm-2.1.2.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/network-2.2.1.7/ghc-6.12.3 -L/Users/simon/.cabal/lib/neither-0.1.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtlparse-0.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/io-storage-0.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/haskeline-0.6.3.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/utf8-string-0.3.6/ghc-6.12.3 -L/Users/simon/.cabal/lib/terminfo-0.3.1.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtl-2.0.1.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/hamlet-0.6.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/file-embed-0.0.3/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/template-haskell-2.4.0.1 -L/usr/local/lib/ghc-6.12.3/pretty-1.0.1.1 -L/Users/simon/.cabal/lib/failure-0.1.0/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/extensible-exceptions-0.1.1.1 -L/Users/simon/.cabal/lib/enumerator-0.4.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/transformers-0.2.2.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/email-validate-0.2.5/ghc-6.12.3 -L/Users/simon/.cabal/lib/ranges-0.2.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/parsec-2.1.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/clientsession-0.4.0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/random-1.0.0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/time-1.2.0.3/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/directory-1.0.1.1 -L/usr/local/lib/ghc-6.12.3/unix-2.4.0.2 -L/usr/local/lib/ghc-6.12.3/old-time-1.0.0.5 -L/usr/local/lib/ghc-6.12.3/old-locale-1.0.0.2 -L/usr/local/lib/ghc-6.12.3/filepath-1.1.0.4 -L/Users/simon/.cabal/lib/cereal-0.3.0.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/blaze-builder-0.2.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/text-0.10.0.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/deepseq-1.1.0.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/binary-0.5.0.2/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/containers-0.3.0.0 -L/Users/simon/.cabal/lib/base64-bytestring-0.1.0.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/bytestring-0.9.1.7 -L/usr/local/lib/ghc-6.12.3/base-3.0.3.2 -L/usr/local/lib/ghc-6.12.3/syb-0.1.0.2 -L/usr/local/lib/ghc-6.12.3/array-0.3.0.1 -L/Users/simon/.cabal/lib/HUnit-1.2.2.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/base-4.2.0.2 -L/usr/local/lib/ghc-6.12.3/integer-gmp-0.2.0.1 -L/usr/local/lib/ghc-6.12.3/ghc-prim-0.2.0.0 -L/usr/local/lib/ghc-6.12.3 -lHSrtsmain -lHSyesod-0.6.6 -lHSdata-default-0.2 -lHSxss-sanitize-0.2.4 -lHSweb-routes-0.23.1 -lHSwai-extra-0.2.4.2 -lHSzlib-bindings-0.0.0 -lHSzlib-0.5.2.0 -lz -lHSwai-0.2.0 -lHStagsoup-0.11.1 -lHSsplit-0.1.2 -lHSsafe-0.2 -lHSregexpr-0.5.1 -lHSpureMD5-1.1.0.0 -lHSprocess-1.0.1.3 -lHSpersistent-0.3.0.1 -lHSweb-routes-quasi-0.6.2 -lHSstm-2.1.2.2 -lHSnetwork-2.2.1.7 -lHSneither-0.1.0 -lHSmtlparse-0.0.1 -lHSio-storage-0.3 -lHShaskeline-0.6.3.2 -liconv -lHSutf8-string-0.3.6 -lHSterminfo-0.3.1.3 -lncurses -lHSmtl-2.0.1.0 -lHShamlet-0.6.0.1 -lHSfile-embed-0.0.3 -lHStemplate-haskell-2.4.0.1 -lHSpretty-1.0.1.1 -lHSfailure-0.1.0 -lHSextensible-exceptions-0.1.1.1 -lHSenumerator-0.4.2 -lHStransformers-0.2.2.0 -lHSemail-validate-0.2.5 -lHSranges-0.2.2 -lHSparsec-2.1.0.1 -lHSclientsession-0.4.0.2 -lHSrandom-1.0.0.2 -lHStime-1.2.0.3 -lHSdirectory-1.0.1.1 -lHSunix-2.4.0.2 -ldl -lHSold-time-1.0.0.5 -lHSold-locale-1.0.0.2 -lHSfilepath-1.1.0.4 -lHScereal-0.3.0.0 -lHSblaze-builder-0.2.0.1 -lHStext-0.10.0.0 -lHSdeepseq-1.1.0.0 -lHSbinary-0.5.0.2 -lHScontainers-0.3.0.0 -lHSbase64-bytestring-0.1.0.1 -lHSbytestring-0.9.1.7 -lHSbase-3.0.3.2 -lHSsyb-0.1.0.2 -lHSarray-0.3.0.1 -lHSHUnit-1.2.2.1 -lHSbase-4.2.0.2 -liconv -lHSinteger-gmp-0.2.0.1 -lHSghc-prim-0.2.0.0 -lHSrts -lm -ldl -u _ghczmprim_GHCziTypes_Izh_static_info -u _ghczmprim_GHCziTypes_Czh_static_info -u _ghczmprim_GHCziTypes_Fzh_static_info -u _ghczmprim_GHCziTypes_Dzh_static_info -u _base_GHCziPtr_Ptr_static_info -u _base_GHCziWord_Wzh_static_info -u _base_GHCziInt_I8zh_static_info -u _base_GHCziInt_I16zh_static_info -u _base_GHCziInt_I32zh_static_info -u _base_GHCziInt_I64zh_static_info -u _base_GHCziWord_W8zh_static_info -u _base_GHCziWord_W16zh_static_info -u _base_GHCziWord_W32zh_static_info -u _base_GHCziWord_W64zh_static_info -u _base_GHCziStable_StablePtr_static_info -u _ghczmprim_GHCziTypes_Izh_con_info -u _ghczmprim_GHCziTypes_Czh_con_info -u _ghczmprim_GHCziTypes_Fzh_con_info -u _ghczmprim_GHCziTypes_Dzh_con_info -u _base_GHCziPtr_Ptr_con_info -u _base_GHCziPtr_FunPtr_con_info -u _base_GHCziStable_StablePtr_con_info -u _ghczmprim_GHCziBool_False_closure -u _ghczmprim_GHCziBool_True_closure -u _base_GHCziPack_unpackCString_closure -u _base_GHCziIOziException_stackOverflow_closure -u _base_GHCziIOziException_heapOverflow_closure -u _base_ControlziExceptionziBase_nonTermination_closure -u _base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -u _base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -u _base_ControlziExceptionziBase_nestedAtomically_closure -u _base_GHCziWeak_runFinalizzerBatch_closure -u _base_GHCziTopHandler_runIO_closure -u _base_GHCziTopHandler_runNonIO_closure -u _base_GHCziConc_ensureIOManagerIsRunning_closure -u _base_GHCziConc_runSparks_closure -u _base_GHCziConc_runHandlers_closure -Wl,-search_paths_first -read_only_relocs warning -lHSffi
	-ghc -c --make hledger-vty/hledger-vty.hs $(MACRELEASEBUILDFLAGS) -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-vty/'`
	-gcc -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-vty/'` -march=i686 -m32 hledger-vty/hledger-vty.o hledger-vty/Hledger/Vty/Main.o hledger/Hledger/Cli/Balance.o hledger/Hledger/Cli/Options.o hledger/Hledger/Cli/Print.o hledger/Hledger/Cli/Register.o hledger/Hledger/Cli/Utils.o hledger/Hledger/Cli/Version.o hledger-lib/Hledger/Data.o hledger-lib/Hledger/Data/Account.o hledger-lib/Hledger/Data/AccountName.o hledger-lib/Hledger/Data/Amount.o hledger-lib/Hledger/Data/Commodity.o hledger-lib/Hledger/Data/Dates.o hledger-lib/Hledger/Data/Transaction.o hledger-lib/Hledger/Data/Ledger.o hledger-lib/Hledger/Data/Journal.o hledger-lib/Hledger/Data/Posting.o hledger-lib/Hledger/Data/TimeLog.o hledger-lib/Hledger/Data/Types.o hledger-lib/Hledger/Data/Utils.o hledger-lib/Hledger/Read.o hledger-lib/Hledger/Read/Utils.o hledger-lib/Hledger/Read/JournalReader.o hledger-lib/Hledger/Read/TimelogReader.o -L/usr/lib -L/Users/simon/.cabal/lib/vty-4.6.0.1/ghc-6.12.3 -L/usr/lib -L/opt/local/lib -L/Users/simon/.cabal/lib/parallel-2.2.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/utf8-string-0.3.6/ghc-6.12.3 -L/Users/simon/.cabal/lib/time-1.2.0.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/terminfo-0.3.1.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/split-0.1.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/safe-0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/regexpr-0.5.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/process-1.0.1.3 -L/Users/simon/.cabal/lib/parsec-2.1.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtlparse-0.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtl-2.0.1.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/transformers-0.2.2.0/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/extensible-exceptions-0.1.1.1 -L/usr/local/lib/ghc-6.12.3/directory-1.0.1.1 -L/usr/local/lib/ghc-6.12.3/unix-2.4.0.2 -L/usr/local/lib/ghc-6.12.3/old-time-1.0.0.5 -L/usr/local/lib/ghc-6.12.3/old-locale-1.0.0.2 -L/usr/local/lib/ghc-6.12.3/filepath-1.1.0.4 -L/Users/simon/.cabal/lib/deepseq-1.1.0.0/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/containers-0.3.0.0 -L/usr/local/lib/ghc-6.12.3/bytestring-0.9.1.7 -L/usr/local/lib/ghc-6.12.3/base-3.0.3.2 -L/usr/local/lib/ghc-6.12.3/syb-0.1.0.2 -L/usr/local/lib/ghc-6.12.3/array-0.3.0.1 -L/Users/simon/.cabal/lib/HUnit-1.2.2.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/base-4.2.0.2 -L/usr/local/lib/ghc-6.12.3/integer-gmp-0.2.0.1 -L/usr/local/lib/ghc-6.12.3/ghc-prim-0.2.0.0 -L/usr/local/lib/ghc-6.12.3 -lHSrtsmain -lHSvty-4.6.0.1 -lHSparallel-2.2.0.1 -lHSutf8-string-0.3.6 -lHStime-1.2.0.3 -lHSterminfo-0.3.1.3 -lncurses -lHSsplit-0.1.2 -lHSsafe-0.2 -lHSregexpr-0.5.1 -lHSprocess-1.0.1.3 -lHSparsec-2.1.0.1 -lHSmtlparse-0.0.1 -lHSmtl-2.0.1.0 -lHStransformers-0.2.2.0 -lHSextensible-exceptions-0.1.1.1 -lHSdirectory-1.0.1.1 -lHSunix-2.4.0.2 -ldl -lHSold-time-1.0.0.5 -lHSold-locale-1.0.0.2 -lHSfilepath-1.1.0.4 -lHSdeepseq-1.1.0.0 -lHScontainers-0.3.0.0 -lHSbytestring-0.9.1.7 -lHSbase-3.0.3.2 -lHSsyb-0.1.0.2 -lHSarray-0.3.0.1 -lHSHUnit-1.2.2.1 -lHSbase-4.2.0.2 -liconv -lHSinteger-gmp-0.2.0.1 -lHSghc-prim-0.2.0.0 -lHSrts -lm -ldl -u _ghczmprim_GHCziTypes_Izh_static_info -u _ghczmprim_GHCziTypes_Czh_static_info -u _ghczmprim_GHCziTypes_Fzh_static_info -u _ghczmprim_GHCziTypes_Dzh_static_info -u _base_GHCziPtr_Ptr_static_info -u _base_GHCziWord_Wzh_static_info -u _base_GHCziInt_I8zh_static_info -u _base_GHCziInt_I16zh_static_info -u _base_GHCziInt_I32zh_static_info -u _base_GHCziInt_I64zh_static_info -u _base_GHCziWord_W8zh_static_info -u _base_GHCziWord_W16zh_static_info -u _base_GHCziWord_W32zh_static_info -u _base_GHCziWord_W64zh_static_info -u _base_GHCziStable_StablePtr_static_info -u _ghczmprim_GHCziTypes_Izh_con_info -u _ghczmprim_GHCziTypes_Czh_con_info -u _ghczmprim_GHCziTypes_Fzh_con_info -u _ghczmprim_GHCziTypes_Dzh_con_info -u _base_GHCziPtr_Ptr_con_info -u _base_GHCziPtr_FunPtr_con_info -u _base_GHCziStable_StablePtr_con_info -u _ghczmprim_GHCziBool_False_closure -u _ghczmprim_GHCziBool_True_closure -u _base_GHCziPack_unpackCString_closure -u _base_GHCziIOziException_stackOverflow_closure -u _base_GHCziIOziException_heapOverflow_closure -u _base_ControlziExceptionziBase_nonTermination_closure -u _base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -u _base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -u _base_ControlziExceptionziBase_nestedAtomically_closure -u _base_GHCziWeak_runFinalizzerBatch_closure -u _base_GHCziTopHandler_runIO_closure -u _base_GHCziTopHandler_runNonIO_closure -u _base_GHCziConc_ensureIOManagerIsRunning_closure -u _base_GHCziConc_runSparks_closure -u _base_GHCziConc_runHandlers_closure -Wl,-search_paths_first -read_only_relocs warning -lHSffi
	-ghc -c --make hledger-chart/hledger-chart.hs $(MACRELEASEBUILDFLAGS) -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-chart/'`
	-gcc -o bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-chart/'` -march=i686 -m32 hledger-chart/hledger-chart.o hledger-chart/Hledger/Chart/Main.o hledger/Hledger/Cli/Options.o hledger/Hledger/Cli/Utils.o hledger/Hledger/Cli/Version.o hledger-lib/Hledger/Data.o hledger-lib/Hledger/Data/Account.o hledger-lib/Hledger/Data/AccountName.o hledger-lib/Hledger/Data/Amount.o hledger-lib/Hledger/Data/Commodity.o hledger-lib/Hledger/Data/Dates.o hledger-lib/Hledger/Data/Transaction.o hledger-lib/Hledger/Data/Ledger.o hledger-lib/Hledger/Data/Journal.o hledger-lib/Hledger/Data/Posting.o hledger-lib/Hledger/Data/TimeLog.o hledger-lib/Hledger/Data/Types.o hledger-lib/Hledger/Data/Utils.o hledger-lib/Hledger/Read.o hledger-lib/Hledger/Read/Utils.o hledger-lib/Hledger/Read/JournalReader.o hledger-lib/Hledger/Read/TimelogReader.o -L/usr/lib -L/Users/simon/.cabal/lib/utf8-string-0.3.6/ghc-6.12.3 -L/opt/local/lib -L/Users/simon/.cabal/lib/split-0.1.2/ghc-6.12.3 -L/usr/lib -L/Users/simon/.cabal/lib/safe-0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/regexpr-0.5.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/process-1.0.1.3 -L/Users/simon/.cabal/lib/parsec-2.1.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtlparse-0.0.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/HUnit-1.2.2.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/Chart-0.14/ghc-6.12.3 -L/Users/simon/.cabal/lib/gtk-0.12.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/pango-0.12.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/gio-0.12.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/glib-0.12.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/data-accessor-template-0.2.1.5/ghc-6.12.3 -L/Users/simon/.cabal/lib/utility-ht-0.0.5.1/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/base-3.0.3.2 -L/usr/local/lib/ghc-6.12.3/syb-0.1.0.2 -L/usr/local/lib/ghc-6.12.3/template-haskell-2.4.0.1 -L/usr/local/lib/ghc-6.12.3/pretty-1.0.1.1 -L/Users/simon/.cabal/lib/data-accessor-0.2.1.4/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/containers-0.3.0.0 -L/Users/simon/.cabal/lib/colour-2.3.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/cairo-0.12.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/mtl-2.0.1.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/transformers-0.2.2.0/ghc-6.12.3 -L/Users/simon/.cabal/lib/haskell98-1.0.1.1/ghc-6.12.3 -L/Users/simon/.cabal/lib/random-1.0.0.2/ghc-6.12.3 -L/Users/simon/.cabal/lib/time-1.2.0.3/ghc-6.12.3 -L/Users/simon/.cabal/lib/process-1.0.1.4/ghc-6.12.3 -L/usr/local/lib/ghc-6.12.3/directory-1.0.1.1 -L/usr/local/lib/ghc-6.12.3/unix-2.4.0.2 -L/usr/local/lib/ghc-6.12.3/old-time-1.0.0.5 -L/usr/local/lib/ghc-6.12.3/old-locale-1.0.0.2 -L/usr/local/lib/ghc-6.12.3/filepath-1.1.0.4 -L/usr/local/lib/ghc-6.12.3/bytestring-0.9.1.7 -L/usr/local/lib/ghc-6.12.3/array-0.3.0.1 -L/usr/local/lib/ghc-6.12.3/base-4.2.0.2 -L/usr/local/lib/ghc-6.12.3/integer-gmp-0.2.0.1 -L/usr/local/lib/ghc-6.12.3/ghc-prim-0.2.0.0 -L/usr/local/lib/ghc-6.12.3 -lHSrtsmain -lHSutf8-string-0.3.6 -lHSsplit-0.1.2 -lHSsafe-0.2 -lHSregexpr-0.5.1 -lHSprocess-1.0.1.3 -lHSparsec-2.1.0.1 -lHSmtlparse-0.0.1 -lHSHUnit-1.2.2.1 -lHSChart-0.14 -lHSgtk-0.12.0 -lgtk-x11-2.0 -lgdk-x11-2.0 -latk-1.0 -lpangocairo-1.0 -lXinerama -lXi -lXrandr -lXcursor -lXcomposite -lXdamage -lgdk_pixbuf-2.0 -lpangoft2-1.0 -lgio-2.0 -lXext -lXfixes -lcairo -lpixman-1 -lXrender -lX11 -lxcb -lXau -lXdmcp -lpng12 -lpango-1.0 -lm -lfontconfig -lexpat -lfreetype -lz -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl -liconv -lHSpango-0.12.0 -lpangocairo-1.0 -lcairo -lpangoft2-1.0 -lpixman-1 -lpng12 -lXrender -lX11 -lxcb -lXau -lXdmcp -lpango-1.0 -lm -lfontconfig -lexpat -lfreetype -lz -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl -liconv -lHSgio-0.12.0 -lgio-2.0 -lz -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl -liconv -lHSglib-0.12.0 -lgobject-2.0 -lgthread-2.0 -lglib-2.0 -lintl -liconv -lHSdata-accessor-template-0.2.1.5 -lHSutility-ht-0.0.5.1 -lHSbase-3.0.3.2 -lHSsyb-0.1.0.2 -lHStemplate-haskell-2.4.0.1 -lHSpretty-1.0.1.1 -lHSdata-accessor-0.2.1.4 -lHScontainers-0.3.0.0 -lHScolour-2.3.1 -lHScairo-0.12.0 -lcairo -lgobject-2.0 -lpixman-1 -lfontconfig -lexpat -lfreetype -lpng12 -lz -lXrender -lgthread-2.0 -lglib-2.0 -lintl -liconv -lX11 -lxcb -lXau -lXdmcp -lHSmtl-2.0.1.0 -lHStransformers-0.2.2.0 -lHShaskell98-1.0.1.1 -lHSrandom-1.0.0.2 -lHStime-1.2.0.3 -lHSprocess-1.0.1.4 -lHSdirectory-1.0.1.1 -lHSunix-2.4.0.2 -ldl -lHSold-time-1.0.0.5 -lHSold-locale-1.0.0.2 -lHSfilepath-1.1.0.4 -lHSbytestring-0.9.1.7 -lHSarray-0.3.0.1 -lHSbase-4.2.0.2 -liconv -lHSinteger-gmp-0.2.0.1 -lHSghc-prim-0.2.0.0 -lHSrts -lm -ldl -u _ghczmprim_GHCziTypes_Izh_static_info -u _ghczmprim_GHCziTypes_Czh_static_info -u _ghczmprim_GHCziTypes_Fzh_static_info -u _ghczmprim_GHCziTypes_Dzh_static_info -u _base_GHCziPtr_Ptr_static_info -u _base_GHCziWord_Wzh_static_info -u _base_GHCziInt_I8zh_static_info -u _base_GHCziInt_I16zh_static_info -u _base_GHCziInt_I32zh_static_info -u _base_GHCziInt_I64zh_static_info -u _base_GHCziWord_W8zh_static_info -u _base_GHCziWord_W16zh_static_info -u _base_GHCziWord_W32zh_static_info -u _base_GHCziWord_W64zh_static_info -u _base_GHCziStable_StablePtr_static_info -u _ghczmprim_GHCziTypes_Izh_con_info -u _ghczmprim_GHCziTypes_Czh_con_info -u _ghczmprim_GHCziTypes_Fzh_con_info -u _ghczmprim_GHCziTypes_Dzh_con_info -u _base_GHCziPtr_Ptr_con_info -u _base_GHCziPtr_FunPtr_con_info -u _base_GHCziStable_StablePtr_con_info -u _ghczmprim_GHCziBool_False_closure -u _ghczmprim_GHCziBool_True_closure -u _base_GHCziPack_unpackCString_closure -u _base_GHCziIOziException_stackOverflow_closure -u _base_GHCziIOziException_heapOverflow_closure -u _base_ControlziExceptionziBase_nonTermination_closure -u _base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -u _base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -u _base_ControlziExceptionziBase_nestedAtomically_closure -u _base_GHCziWeak_runFinalizzerBatch_closure -u _base_GHCziTopHandler_runIO_closure -u _base_GHCziTopHandler_runNonIO_closure -u _base_GHCziConc_ensureIOManagerIsRunning_closure -u _base_GHCziConc_runSparks_closure -u _base_GHCziConc_runHandlers_closure -Wl,-search_paths_first -read_only_relocs warning -lHSffi
	@echo 'Please check the binaries look portable, then make compressbinaries:'
	otool -L bin/*`arch`

# build deployable binaries for windows, assuming cygwin tools are present
hledgerwin: install
	cp ~/.cabal/bin/hledger.exe bin/`echo $(BINARYFILENAME) | dos2unix`
	-cp ~/.cabal/bin/hledger-web.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/' | dos2unix`
	-cp ~/.cabal/bin/hledger-vty.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-vty/' | dos2unix`
	-cp ~/.cabal/bin/hledger-chart.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-chart/' | dos2unix`
	@echo 'Please check the binaries look portable, then zip them:'
	ls -l bin/*`arch`

compressbinaries:
	gzip bin/*`arch`

######################################################################
# TESTING

test: codetest

# quick code tests - run all the time
codetest: unittest functest

# moderate pre-commit tests - run before record or before send/push, your choice
committest: hlinttest unittest doctest functest haddocktest warningstest quickcabaltest

# thorough pre-release tests - run before release
# consider hiding dev-build symlinks in Hledger/ first
releasetest: Clean unittest doctest functest warningstest fullcabaltest haddocktest

hlinttest hlint:
	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

# run unit tests
unittest: unittest-builtin

unittest-builtin: bin/hledger
	@(bin/hledger test \
		&& echo $@ PASSED) || echo $@ FAILED

# XXX doesn't rebuild on hledger source changes
unittest-standalone: tools/unittest
	@(tools/unittest \
		&& echo $@ PASSED) || echo $@ FAILED

# run unit tests without waiting for compilation
unittesths:
	@(runghc $(MAIN) test \
		&& echo $@ PASSED) || echo $@ FAILED

# run functional tests, requires shelltestrunner >= 0.9 from hackage
# 16 threads sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" here but seems harmless
functest: bin/hledger
	(shelltest tests -- --threads=16 --hide-successes \
		&& echo $@ PASSED) || echo $@ FAILED

# run doc tests
DOCTESTFILES=\
	hledger/Hledger/Cli/Tests.hs
doctest: tools/doctest
	@for f in $(DOCTESTFILES); do \
		(tools/doctest $$f && echo $@ PASSED) || echo $@ FAILED ; done

# make sure we have no haddock errors
haddocktest:
	@(make --quiet haddock \
		&& echo $@ PASSED) || echo $@ FAILED

# needs updating
# make sure the normal build has no warnings
warningstest:
	@(make -s clean \
		&& make --no-print-directory -s hledgernowarnings \
		&& echo $@ PASSED) || echo $@ FAILED

# make sure cabal is reasonably happy
quickcabaltest:
	@(make --no-print-directory allcabalclean allcabalcheck allcabalconfigure \
		&& echo $@ PASSED) || echo $@ FAILED

# make sure cabal is happy in all possible ways
fullcabaltest:
	(for p in $(PACKAGES); do (echo "testing $$p package" && cd $$p && cabal clean && cabal check && cabal install && cabal sdist && cabal upload dist/$$p-$(VERSION).tar.gz --check -v3); done \
		&& echo $@ PASSED) || echo $@ FAILED

# run simple performance benchmarks without saving results
# Requires some commands defined in bench.tests and some BENCHEXES defined above.
quickbench: samplejournals bench.tests tools/simplebench
	tools/simplebench -fbench.tests $(BENCHEXES)
	@rm -f benchresults.*

# run simple performance benchmarks and archive results
# Requires some commands defined in bench.tests and some BENCHEXES defined above.
simplebench: samplejournals bench.tests tools/simplebench
	tools/simplebench -fbench.tests $(BENCHEXES) | tee profs/$(TIME).bench
	@rm -f benchresults.*
	@(cd profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

# run criterion benchmark tests and save graphical results
criterionbench: samplejournals tools/criterionbench
	tools/criterionbench -t png -k png

# run progression benchmark tests and save graphical results
progressionbench: samplejournals tools/progressionbench
	tools/progressionbench -- -t png -k png

# generate and archive an execution profile
prof: samplejournals hledgerp
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS -p -RTS
	mv hledgerp.prof profs/$(TIME).prof
	(cd profs; rm -f latest*.prof; ln -s $(TIME).prof latest.prof)

# generate, archive, simplify and display an execution profile
viewprof: prof
	tools/simplifyprof.hs profs/latest.prof

# generate and display an execution profile, don't save or simplify
quickprof: samplejournals hledgerp
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS -p -RTS
	echo; cat hledgerp.prof

# generate and archive a graphical heap profile
heap: samplejournals hledgerp
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	mv hledgerp.hp profs/$(TIME).hp
	(cd profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)

viewheap: heap
	$(VIEWPS) profs/latest.ps

# generate and display a graphical heap profile, don't save
quickheap: samplejournals hledgerp
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	hp2ps hledgerp.hp
	$(VIEWPS) hledger.ps

# display a code coverage text report from running hledger COVCMD
quickcoverage:
	@echo "Generating code coverage text report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "report" $(COVCMD)

# generate a code coverage html report from running hledger COVCMD
coverage: samplejournals hledgerhpc
	@echo "Generating code coverage html report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "markup --destdir=profs/coverage" $(COVCMD)
	cd profs/coverage; rm -f index.html; ln -s hpc_index.html index.html

# view the last html code coverage report
viewcoverage:
	$(VIEWHTML) profs/coverage/index.html

# get a debug prompt
ghci:
	ghci $(INCLUDEPATHS) $(MAIN)

ghci-vty:
	ghci $(INCLUDEPATHS) hledger-vty/Hledger/Vty/Main.hs

# generate standard sample journals
samplejournals: data/sample.journal data/100x100x10.journal data/1000x1000x10.journal data/10000x1000x10.journal data/100000x1000x10.journal

data/sample.journal:
	true # XXX should probably regenerate this

data/100x100x10.journal: tools/generatejournal
	tools/generatejournal 100 100 10 >$@

data/1000x1000x10.journal: tools/generatejournal
	tools/generatejournal 1000 1000 10 >$@

data/10000x1000x10.journal: tools/generatejournal
	tools/generatejournal 10000 1000 10 >$@

data/100000x1000x10.journal: tools/generatejournal
	tools/generatejournal 100000 1000 10 >$@

######################################################################
# DOCUMENTATION

# Documentation source files are UPPERCASE files in the top directory.
# site/ contains both html generated from these (UPPERCASE.html) and
# revision-controlled resource files (everything else).  site/api
# contains only generated files.

cleandocs:
	rm -rf site/[A-Z]*.html site/api/*

# rebuild all docs
docs: site codedocs

# build the hledger.org website
# Requires hakyll (cabal install hakyll)
.PHONY: site
site: site/hakyll
	cd site; ./hakyll build

cleansite: site/hakyll
	cd site; ./hakyll clean

previewsite: site/hakyll
	cd site; ./hakyll preview

site/hakyll: site/hakyll.hs
	cd site; ghc --make hakyll.hs $(PREFERMACUSRLIBFLAGS)

autosite:
	cd site; $(AUTOBUILD) hakyll.hs -o hakyll $(PREFERMACUSRLIBFLAGS) --run preview

viewsite: site
	$(VIEWHTML) site/_site/index.html

# called on each darcs commit
commithook: site

# generate html versions of docs (and the hledger.org website)
# work around pandoc not handling full rst image directive
# html:
# 	for d in $(DOCFILES); do $(PANDOC) --toc -s -H site/header.html -A site/footer.html -r rst $$d >site/$$d.html; done
# 	cd site && ln -sf ../SCREENSHOTS && $(RST2HTML) SCREENSHOTS >SCREENSHOTS.html && rm -f SCREENSHOTS
# 	cd site; rm -f index.html; ln -s README.html index.html; rm -f profs; ln -s ../profs


pdf: docspdf codepdf

# generate pdf versions of main docs
# docspdf:
# 	-for d in $(DOCFILES); do (cd site && ln -sf ../$$d && pandoc $$d -w pdf && rm -f $$d); done

# format all code as a pdf for offline reading
ENSCRIPT=enscript -q --header='$$n|$$D{%+}|Page $$% of $$=' --highlight=haskell --line-numbers --font=Courier6 --color -o-
codepdf:
	$(ENSCRIPT) --pretty-print=makefile hledger.cabal >cabal.ps
	$(ENSCRIPT) --pretty-print=makefile Makefile >make.ps
	$(ENSCRIPT) --pretty-print=haskell $(SOURCEFILES) >haskell.ps
	cat cabal.ps make.ps haskell.ps | ps2pdf - >code.pdf

# view all docs and code as pdf
PDFS=site/{README,README2,MANUAL,NEWS,CONTRIBUTORS,SCREENSHOTS}.pdf code.pdf
viewall: pdf
	$(VIEWPDF) $(PDFS)

# print all docs and code for offline reading
printall: pdf
	$(PRINT) $(PDFS)

# push latest docs etc. and update the hledger.org site
pushdocs: push
	ssh simon@joyful.com 'make -C/repos/hledger docs'

# dump all executables' command line help into files for review
EXES=hledger hledger-vty hledger-web hledger-chart
savehelp:
	for e in $(EXES); do $$e --help >.HELP_$$e; done

# generate api & other code docs
codedocs: haddock hscolour coverage #sourcegraph #hoogle

#http://www.haskell.org/haddock/doc/html/invoking.html
#$(subst -D,--optghc=-D,$(DEFINEFLAGS))
HADDOCK=haddock --no-warnings --prologue .haddockprologue #--optghc='-hide-package monads-tf' 

.haddocksynopsis: hledger/hledger.cabal
	grep synopsis $< | sed -e 's/synopsis: *//' >$@

.haddockprologue: hledger/hledger.cabal
	cat $< | perl -ne 'print if (/^description:/../^$$/)' | sed -e 's/^description: *//' >$@
	printf "\nThis haddock covers all hledger-* packages, for individual package haddocks see hackage.\n" >>$@

# generate api docs for the whole project
haddock: linkhledgerwebdir .haddockprologue
	$(HADDOCK) --title "hledger-* API docs" \
	 -o site/api \
	 --html \
	 --source-module=src/%{MODULE/./-}.html \
	 --source-entity=src/%{MODULE/./-}.html#%N \
	 $(HADDOCKSOURCEFILES)

# browse the api docs
viewhaddock:
	$(VIEWHTML) site/api/index.html

# http://www.cs.york.ac.uk/fp/darcs/hscolour/
HSCOLOUR=HsColour -css
hscolour: site/api/src site/api/src/hscolour.css
	for f in $(HADDOCKSOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -osite/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done

site/api/src/hscolour.css: site/api/src
	$(HSCOLOUR) -print-css >site/api/src/hscolour.css

site/api/src:
	mkdir -p site/api/src

sourcegraph:
	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

# # generate external api docs for each package
# allhaddock: allcabalhaddock\ --hyperlink-source\ --executables

# # generate internal code docs for each package
# allhaddockinternal: allcabalhaddock\ --hyperlink-source\ --executables\ --internal

# # generate hoogle indices for each package
# allhoogle: allcabalhaddock\ --hoogle\ --executables

#set up the hoogle web interface
## We munge haddock and hoogle into a rough but useful framed layout.
## For this to work the hoogle cgi must be built with base target "main".
## XXX move the framed index building into haddock: ?
# 	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' site/api/modules-index.html ; \
# 	cp site/api-frames.html site/api/index.html ; \
# # 	cp site/hoogle-small.html site/api
#
#uses a hoogle source tree configured with --datadir=., patched to fix haddock urls/target frame
# HOOGLESRC=/usr/local/src/hoogle
# HOOGLE=$(HOOGLESRC)/dist/build/hoogle/hoogle
# HOOGLEVER=`$(HOOGLE) --version |tail -n 1 | sed -e 's/Version /hoogle-/'`
# hoogle: hoogleindex
# 	if test -f $(HOOGLE) ; then \
# 		cd site/api && \
# 		rm -f $(HOOGLEVER) && \
# 		ln -s . $(HOOGLEVER) && \
# 		cp -r $(HOOGLESRC)/src/res/ . && \
# 		cp -p $(HOOGLE) index.cgi && \
# 		touch log.txt && chmod 666 log.txt ; \
# 	else \
# 		echo "Could not find $(HOOGLE) in the hoogle source tree" ; \
# 	fi
#
#generate a hoogle index
# hoogleindex:
# 	$(HADDOCK) -o site/api --hoogle $(MAIN) && \
# 	cd site/api && \
# 	hoogle --convert=main.txt --output=default.hoo

######################################################################
# RELEASING

# Version numbering. See also VERSION and Version.hs.
#
# hledger's version number appears in:
#  hledger --version
#  hledger's cabal file
#  darcs tags
#  hackage tarball filenames
#  hackage pages
#
# Some old version numbering goals:
# 1 automation, robustness, simplicity, platform independence
# 2 cabal versions must be all-numeric
# 3 release versions can be concise (without extra .0's)
# 4 releases should have a corresponding darcs tag
# 5 development builds should have a precise version appearing in --version
# 6 development builds should generate cabal packages with non-confusing versions
# 7 there should be a way to mark builds/releases as alpha or beta
# 8 it should be easy to darcs get the .0 release even after bugfix releases
# 9 avoid unnecessary compiling and linking
# 10 minimise rcs noise and syncing issues (commits, unrecorded changes)
#
# Current policy:
#
# - We follow http://haskell.org/haskellwiki/Package_versioning_policy
#
# - The full release version is ma.jor.minor, where minor is 0 for a
#   normal release or 1..n for bugfix releases.
#
# - The elided release version is ma.jor when minor is 0. We use it for
#   hackage releases when possible, trusting it doesn't cause trouble..
#
# - The build version is ma.jor.minor+patches, where patches is the number
#   of patches applied in the current repo since the last release tag.
#
# - The release tag in the repo is the full release version.
#
# - hledger --version shows the release version or build version as
#   appropriate.
#
# - The VERSION file must be updated manually before a release.
#
# - "make simplebench" depends on version numbers in BENCHEXES, these also
#   must be updated manually.
#
# - "make" updates the version in most other places, and defines PATCHES.
#   Note "cabal build" should also do this but doesn't yet.
#
# - "make release" additionally records the main version number-affected
#   files, and tags the repo with the release tag.

# Build a release, tag the repo, prepare a cabal package
# First update VERSION. Eg:
# a normal release: echo 0.7   >VERSION; make release
# a bugfix release: echo 0.7.1 >VERSION; make release
release: releasetest setandrecordversion tagrelease

# Upload the latest cabal package and update hledger.org
upload: allcabalsdist hackageupload pushdocs

releaseandupload: release upload


# update the version number in local files, and prompt to record changes
# in these files. Triggered by "make release".
setandrecordversion: setversion
	darcs record -m "bump version" $(VERSIONFILE) $(VERSIONSENSITIVEFILES)

# update the version string in local files. This should be run immediately
# after editing the VERSION file.
setversion: $(VERSIONSENSITIVEFILES)

# re-update version string even if it seems unchanged
Setversion:
	touch $(VERSIONFILE); make setversion

hledger/Hledger/Cli/Version.hs: $(VERSIONFILE)
	perl -p -e "s/(^version *= *)\".*?\"/\1\"$(VERSION3)\"/" -i $@

hledger-lib/hledger-lib.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@

hledger/hledger.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@

hledger-chart/hledger-chart.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@

hledger-vty/hledger-vty.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@

hledger-web/hledger-web.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@

hledger-web/.hledger/web/.version: $(VERSIONFILE)
	cat $(VERSIONFILE) >$@

MANUAL.markdown: $(VERSIONFILE)
	perl -p -e "s/(^This is the.*?manual for hledger.*?) +[0-9.]+/\1 $(VERSION3)./" -i $@

DOWNLOAD.markdown: $(VERSIONFILE)
	perl -p -e "s/hledger(|-chart|-web|-vty)-[0-9.]+-/hledger\1-$(VERSION)-/g" -i $@

tagrelease:
	darcs tag $(VERSION3)

# display a hackage upload command reminder
hackageupload:
	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v3; done

# send unpushed patches to the mail list
send:
	darcs send http://joyful.com/repos/hledger --to=hledger@googlegroups.com --edit-description  

# push patches and anything else pending to the public server
push: pushprofs pushbinary
	darcs push simon@joyful.com:/repos/hledger

# pull anything pending from the public server
pull: pullprofs
	darcs pull -a simon@joyful.com:/repos/hledger

# push any new profiles and benchmark results to the public site
# beware, results will vary depending on which machine generated them
pushprofs:
	rsync -azP profs/ simon@joyful.com:/repos/hledger/profs/

# fetch any new profiles and benchmark results from the public site
pullprofs:
	rsync -azP simon@joyful.com:/repos/hledger/profs/ profs/

# compress the just-built platform binary. make hledgerPLATFORM first. Use
# the win variant on windows.
compressbinary:
	gzip -9 bin/$(BINARYFILENAME)
compressbinarywin:
	cd bin; zip -9 $(BINARYFILENAME).zip $(BINARYFILENAME)

# push the last-updated platform binary to the public download directory
pushbinary:
	cd bin; rsync -aP `ls -t | head -1` simon@joyful.com:/repos/hledger/site/download/


# show project stats useful for release notes
showreleasestats stats: \
	showreleasedays \
	showunreleasedchangecount \
	showloc \
	showtestcount \
	showunittestcoverage \
	showreleaseauthors \
	showunreleasedcodechanges \
	showunpushedchanges
#	simplebench
#	showerrors

showreleasedays:
	@echo Days since last release:
	@tools/dayssincerelease.hs | head -1 | cut -d' ' -f-1
	@echo

showunreleasedchangecount:
	@echo Commits since last release:
	@darcs changes --from-tag . --count
	@echo

showreleaseauthors:
	@echo Patch authors since last release:
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc sloccount:
	@echo Current lines of code including tests:
	@sloccount `ls $(SOURCEFILES)` | grep haskell:
	@echo

showtestcount:
	@echo "Unit tests:"
	@hledger test 2>&1 | cut -d' ' -f2
	@echo "Functional tests:"
	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
	@echo

showunittestcoverage:
	@echo Unit test coverage:
	@make --no-print quickcoverage | grep 'expressions'
	@echo

# showerrors:
# 	@echo Known errors:
# 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES | grep '^\*\*\* ' | tail +1
# 	@echo

showunpushedchanges unpushed:
	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
	@-darcs push simon@joyful.com:/repos/hledger --dry-run | grep '*' | tac
	@echo

showunreleasedcodechanges unreleased:
	@echo "hledger code changes since last release:"
	@darcs changes --from-tag . --matches "not (name docs: or name doc: or name site: or name tools:)" | grep '*'
	@echo

showcodechanges:
	@echo "hledger code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

######################################################################
# MISCELLANEOUS

# fix permissions (eg after darcs get)
fixperms:
	chmod +x tools/* $(MAIN) hledger-*/Main.hs

tag: emacstags

emacstags:
	-@rm -f TAGS; hasktags -e $(SOURCEFILES) $(CABALFILES) $(WEBFILES) Makefile

clean:
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean cleandocs
	rm -f bin/hledger TAGS tags

