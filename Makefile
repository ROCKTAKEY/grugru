EMACS ?= emacs
TESTINGFILE := test/*.el
TESTEDFILES := *.el
CASK ?= cask
WGET ?= wget
GIT ?= git

ert:
	${CASK} exec ${EMACS} -batch -Q -L . -l $(wildcard ${TESTINGFILE}) \
	-f  ert-run-tests-batch-and-exit

travis:
	${MAKE} test-all

compile:
	${CASK} exec ${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" \
	${TESTEDFILES}

clean:
	rm -f ${addsuffix c, ${TESTEDFILES}}

test-all:
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean


.PHONY: ert travis compile clean  test-all
