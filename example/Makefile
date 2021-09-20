GHC=stack exec ghc --
PROFITEUR=profiteur
EVENTLOG2PROF=eventlog2prof

GHC=ghc-9.2.0.20210821
PROFITEUR=/work/iohk/profiteur/dist-newstyle/build/x86_64-linux/ghc-8.6.5/profiteur-0.4.6.0/x/profiteur/build/profiteur/profiteur

all : binary-trees.prof.html binary-trees.eventlog.html binary-trees.eventlog.prof

binary-trees.prof.html: binary-trees.prof
	$(PROFITEUR) binary-trees.prof

binary-trees.eventlog.html: binary-trees.eventlog
	$(PROFITEUR) binary-trees.eventlog

binary-trees.eventlog.prof : binary-trees.eventlog
	$(EVENTLOG2PROF) binary-trees.eventlog > binary-trees.eventlog.prof

binary-trees.prof binary-trees.eventlog: binary-trees
	./binary-trees 10 +RTS -l -P -RTS

binary-trees:
	$(GHC) --make -eventlog -fprof-auto -prof -rtsopts binary-trees.hs

clean:
	rm -f binary-trees
	rm -f binary-trees.o
	rm -f binary-trees.hi
	rm -f binary-trees.prof
	rm -f binary-trees.eventlog
	rm -f binary-trees.prof.html
	rm -f binary-trees.eventlog.html

.PHONY: clean
