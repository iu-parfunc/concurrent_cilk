
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

#ifeq ($(MACHINECLASS),delta)
  $(info Using ConcurrentCilk-specific FusionTable uploader.)
  CID=1063386764086-36m691p8ugsni9d6d2iuuhkn1mum4vg8.apps.googleusercontent.com
  SEC=P3rhLK4dSzBpFQeSdihToqsM
# else
# endif
TABLE=ConcurrentCilk_Benchmarks
# Note, this table can be found on the web at:
#   https://www.google.com/fusiontables/DataSource?docid=1Jtm_Y7226eb3f7tVSUYLnnYGOjcSrVdUJT688XiA

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

deps: $(DEPS)

rebuild:
	./build_scripts/clean_and_rebuild.sh

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --retry=10 --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name=$(TABLE) --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

PKGS= ./ ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-codespeed
CBLARGS= --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls \
         -j1 --extra-include-dirs=$(HOME)/opt/include --extra-lib-dirs=$(HOME)/opt/lib
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

clean:
	rm -rf ./run-benchmarks.exe 
	rm -rf ./build ./install ./deps/build

dist-clean:
	rm -rf ./dist
	$(CABAL) sandbox delete
