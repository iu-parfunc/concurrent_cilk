
# Configuration stuff
# ----------------------------------------
# Responds to Env vars:
#   WHICHBENCH -- gets passed to hsbencher harness upon "make bench"
#   BENCHARGS  -- same as WHICHBENCH, gets passed to hsbencher harness
#   CABAL -- set executable to use for cabal
#   JENKINS_GHC -- X.Y.Z version number for GHC
# ----------------------------------------

ifeq ($(CABAL),)
  CABAL=cabal
endif
ifeq ($(MACHINECLASS),)
  MACHINECLASS=$(shell hostname -s)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

ifeq ($(JENKINS_GHC),)
  JENKINS_GHC=7.8.3
endif

TRIALS=3
# TRIALS=1
TOP=$(shell pwd)
DEPS=deps/build/lib/libevent.so

# Google API authentication

ifeq ($(MACHINECLASS),cutter)
  $(info Using ConcurrentCilk-specific FusionTable uploader.)
  CID=1063386764086-36m691p8ugsni9d6d2iuuhkn1mum4vg8.apps.googleusercontent.com
  SEC=P3rhLK4dSzBpFQeSdihToqsM
  # Hack, par builds segfault on cutter:
  JFLAG= -j1
else ifeq ($(MACHINECLASS),xmen)
  # Using generic uploader because we're over limit:
  # Generic 1:
  CID=905767673358.apps.googleusercontent.com
  SEC=2a2H57dBggubW1_rqglC7jtK
else ifeq ($(MACHINECLASS),mine)
  # Using generic uploader because we're over limit:
  # Generic 2:
  CID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
  SEC=148aQ08EPpgkb0DiYVLoT9X2
else
  # Generic 3:
  CID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
  SEC=yI8GfZXsHPrW44udqklCHeDH
  JFLAG= -j --ghc-option=-j3
endif


# TABLE=ConcurrentCilk_Benchmarks
#   https://www.google.com/fusiontables/DataSource?docid=1Jtm_Y7226eb3f7tVSUYLnnYGOjcSrVdUJT688XiA
# Changing to a fresh one because of broken indexing:
TABLE=ConcurrentCilk_Benchmarks2
# Note, this table can be found on the web at:


.phony: all dobuild rebuild bench deps
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: deps dobuild 

deps/build/lib/libevent.so: 
	cd deps/libevent; \
        ./autogen.sh; \
	./configure --prefix=$(TOP)/deps/build; \
	make -j; \
	make install

# Currently [2014.10.09] deps and dobuild should be able to run concurrently.
dobuild: 
	./build_scripts/build_libcilk.sh
	@echo "DONE building libcilkrts."

debug:
	(export CCILK_DEBUG=1; $(MAKE) dobuild)

deps: $(DEPS)

rebuild:
	./build_scripts/clean_and_rebuild.sh

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --retry=20 --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name=$(TABLE) --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

PKGS= ./ ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-tool ./HSBencher/hsbencher-codespeed
CBLARGS= $(JFLAG) --disable-documentation --with-ghc=ghc-$(JENKINS_GHC)  \
          --extra-include-dirs=$(HOME)/opt/include --extra-lib-dirs=$(HOME)/opt/lib --force-reinstalls
# force-reinstalls is for when we upgrade versions...

# Weird, segfaults on cutter:
# -j --ghc-option=-j3

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	which -a $(CABAL)
	$(CABAL) sandbox init
	$(CABAL) sandbox hc-pkg list
#	$(CABAL) sandbox hc-pkg unregister hsbencher-analytics || echo ok
#	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) -j --force-reinstalls
#	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
#	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls

	$(CABAL) install $(CBLARGS) --only-dep $(PKGS)
	$(CABAL) install $(CBLARGS) --bindir=. $(PKGS)
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

profile_deps:
	(cd ./gperftools/libunwind-0.99-beta; ./configure --prefix=$(CILK_ROOT); make -j; make install)
	(cd ./gperftools/gperftools-2.1; ./configure --prefix=$(CILK_ROOT); make -j; make install)

clean:
	rm -rf ./run-benchmarks.exe 
	rm -rf ./build ./install 

cleandeps:
	rm -rf ./deps/build
	(cd ./deps/libevent; make distclean)

distclean: cleandeps clean
	rm -rf ./dist 
	$(CABAL) sandbox delete || echo ok

