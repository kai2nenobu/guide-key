EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile guide-key.el
test:
	${CASK} exec ${EMACS} -Q -batch -L . -l test/guide-key-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f guide-key.elc

.PHONY: all compile test clean
