# GHC=stack exec ghc --
GHC=ghc-9.2.0.20210821

all : binary-trees.prof.html binary-trees.eventlog.html

binary-trees.prof.html: binary-trees.prof
	profiteur binary-trees.prof

binary-trees.eventlog.html: binary-trees.eventlog
	profiteur binary-trees.eventlog

binary-trees.prof binary-trees.eventlog: binary-trees
	./binary-trees 10 +RTS -l -p -RTS

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
