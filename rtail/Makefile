.PHONY: build clean clean-all dist fom

VERSION   := $(shell cat rtail.cabal | sed -n -E -e 's/^version[[:space:]]*:[[:space:]]*([0-9.]+)/\1/p')
OSARCH		:= $(shell uname -sm | tr ' ' -)
CABAL_DEV := ~/.cabal/bin/cabal-dev
PROGRAMS  := cabal-dev/bin/*
SOURCES		:= $(shell find . -name "*.hs")
TARBALL		:= dist/rtail-$(VERSION)_$(OSARCH).tar.gz
DEB				:= dist/rtail_$(VERSION)_$(OSARCH).deb

LIBZMQ   := libzmq/lib/libzmq*
ZMQ_BASE := zeromq-3.2.2
ZMQ_TAR  := $(ZMQ_BASE).tar.gz
ZMQ_URL  := http://download.zeromq.org/$(ZMQ_TAR)

default : build

build : $(CABAL_DEV) $(LIBZMQ) $(PROGRAMS)

clean :
	rm -rf cabal-dev
	rm -rf dist

clean-all : clean
	rm -rf $(ZMQ_BASE)
	rm $(ZMQ_TAR)
	rm -rf $(LIBZMQ)

dist : build $(TARBALL) $(DEB)

$(TARBALL) :
	(cd cabal-dev/bin; tar -czf ../../$(TARBALL) rtail*)

$(DEB) :
	@echo "Building rtail .deb..."
	$(MAKE) fpm
	mkdir -p dist/deb/usr/local/bin/
	cp cabal-dev/bin/rtail* dist/deb/usr/local/bin
	fpm -C dist/deb \
			-s dir \
			-t deb \
			-a $(OSARCH) \
			-v $(VERSION) \
			-n rtail \
			./usr/local/bin/rtail \
			./usr/local/bin/rtaild \
			./usr/local/bin/rtailp
	mv rtail_$(VERSION)_$(OSARCH).deb dist

$(PROGRAMS) : $(SOURCES)
	$(CABAL_DEV) install \
		--extra-lib-dirs=`pwd`/libzmq/lib \
		--extra-include-dirs=`pwd`/libzmq/include

$(CABAL_DEV) :
	@echo "Checking for cabal-dev..."
	@($(CABAL_DEV) --numeric-version > /dev/null || cabal install cabal-dev)

$(LIBZMQ) : $(ZMQ_BASE)
	(cd $(ZMQ_BASE); \
		./configure --prefix=`pwd`/../libzmq && \
		$(MAKE) && $(MAKE) install; \
		exit 0)

$(ZMQ_BASE) : $(ZMQ_TAR)
	tar xvfz $(ZMQ_TAR)

$(ZMQ_TAR) :
	curl $(ZMQ_URL) > $(ZMQ_TAR)

fpm :
	@echo "Checking for fpm gem..."
	@(gem list | grep ^fpm > /dev/null || gem install fpm)
