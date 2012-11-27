.PHONY: build clean clean-all dist

VERSION   := $(shell cat rtail.cabal | sed -n -E -e 's/^version[[:space:]]*:[[:space:]]*([0-9.]+)/\1/p')
OSARCH		:= $(shell uname -sm | tr ' ' -)
CABAL_DEV := ~/.cabal/bin/cabal-dev
PROGRAMS  := cabal-dev/bin/*
TARBALL		:= dist/rtail-$(VERSION)_$(OSARCH).tar.gz

LIBZMQ   := libzmq
ZMQ_BASE := zeromq-3.2.2
ZMQ_TAR  := $(ZMQ_BASE).tar.gz
ZMQ_URL  := http://download.zeromq.org/$(ZMQ_TAR)

default : build

build : $(CABAL_DEV) $(LIBZMQ) $(PROGRAMS)

clean :
	rm -r cabal-dev
	rm -r dist

clean-all : clean
	rm -r $(ZMQ_BASE)
	rm $(ZMQ_TAR)
	rm -r $(LIBZMQ)

dist : build $(TARBALL)

$(TARBALL) :
	(cd cabal-dev/bin; tar -czf ../../$(TARBALL) rtail*)

$(PROGRAMS) : *.hs
	$(CABAL_DEV) install \
		--extra-lib-dirs=`pwd`/$(LIBZMQ)/lib \
		--extra-include-dirs=`pwd`/$(LIBZMQ)/include

$(CABAL_DEV) :
	@echo "Checking for cabal-dev..."
	@($(CABAL_DEV) --numeric-version > /dev/null || cabal install cabal-dev)

$(LIBZMQ) : $(ZMQ_BASE)
	(cd $(ZMQ_BASE); \
		make clean; \
		./configure --prefix=`pwd`/../$(LIBZMQ) && \
		make && make install; \
		exit 0)

$(ZMQ_BASE) : $(ZMQ_TAR)
	tar xvfz $(ZMQ_TAR)

$(ZMQ_TAR) :
	curl $(ZMQ_URL) > $(ZMQ_TAR)
