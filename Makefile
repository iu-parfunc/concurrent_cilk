
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
  MACHINECLASS=$(HOSTNAME)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

ifeq ($(JENKINS_GHC),)
  JENKINS_GHC=7.8.3
endif

TRIALS=3
# TRIALS=1
TOP=$(shell pwd)
DEPS=lib/libevent.so

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

.phony: all build rebuild bench
# ----------------------------------------

# TODO: build everything before running/benchmarking:
all: build 

$(DEPS): 
	cd deps/libevent-2.1.4-alpha/; \
		./configure --prefix=$(TOP); \
	 	make; \
		make install	

build: $(DEPS) 
	./build_scripts/build_libcilk.sh

rebuild:
	./build_scripts/clean_and_rebuild.sh

# Run the benchmarks
bench: run-benchmarks.exe
	./run-benchmarks.exe --retry=3 --hostname=$(MACHINECLASS) --runid=$(RUNID) --keepgoing --trials=$(TRIALS) --name=$(TABLE) --fusion-upload --clientid=$(CID) --clientsecret=$(SEC) $(WHICHBENCH) $(BENCHARGS)

run-benchmarks.exe: run-benchmarks.cabal run-benchmarks.hs
	$(CABAL) sandbox init
	$(CABAL) sandbox hc-pkg list
	$(CABAL) sandbox hc-pkg unregister hsbencher-analytics || echo ok
	$(CABAL) install ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) -j --force-reinstalls
	$(CABAL) install --only-dep -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	$(CABAL) install --bindir=. --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls
	./run-benchmarks.exe --help
	./run-benchmarks.exe -l

clean:
	rm -rf ./run-benchmarks.exe ./dist
	rm -rf build
