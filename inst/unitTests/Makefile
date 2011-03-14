# Taken from R-package graph, http://cran.r-project.org/web/packages/graph/
TOP=../../..
RUNIT_DIR=$(shell pwd)
INST_PATH=$(shell dirname ${RUNIT_DIR})
PKG_PATH=$(shell dirname ${INST_PATH})

.PHONY: FORCE
all: install test

install:	
	cd ${TOP}; \
	R CMD INSTALL --clean ${PKG_PATH} && \
	cd ${RUNIT_DIR}

test:
	R --slave < runalltests.R

%.R: FORCE
	./runfile.sh $@

%.r: FORCE
	./runfile.sh $@
