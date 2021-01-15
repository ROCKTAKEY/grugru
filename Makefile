EMACS ?= emacs
TESTINGFILES := test/*.el
TESTEDFILES := *.el
KEG ?= keg
WGET ?= wget
GIT ?= git

ert:
	${KEG} exec ${EMACS} --batch -Q -L . $(addprefix --load=, $(wildcard ${TESTINGFILES})) \
	-f  ert-run-tests-batch-and-exit

travis:
	${MAKE} test-all

compile:
	${KEG} exec ${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" \
	${TESTEDFILES}

clean:
	rm -f ${addsuffix c, ${TESTEDFILES}}

test-all:
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean

lint:
	${KEG} lint

.PHONY: ert travis compile clean  test-all lint
