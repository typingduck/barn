.PHONY: all clean clean-all

CABAL_DEV := ~/.cabal/bin/cabal-dev
PROGRAMS  := cabal-dev/bin/*

LIBZMQ   := libzmq
ZMQ_BASE := zeromq-3.2.2
ZMQ_TAR  := $(ZMQ_BASE).tar.gz
ZMQ_URL  := http://download.zeromq.org/$(ZMQ_TAR)

all : $(CABAL_DEV) $(LIBZMQ) $(PROGRAMS)

clean :
	rm -r cabal-dev
	rm -r dist
	rm -r $(LIBZMQ)

clean-all : clean
	rm -r $(ZMQ_BASE)
	rm $(ZMQ_TAR)

$(PROGRAMS) : *.hs
	$(CABAL_DEV) install \
		--extra-lib-dirs=`pwd`/$(LIBZMQ) \
		--extra-include-dirs=`pwd`/$(LIBZMQ)

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
