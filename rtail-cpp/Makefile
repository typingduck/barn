.PHONY: all clean clean-all

PROGRAMS := rtaild rtailp

LIBZMQ   := libzmq
ZMQ_BASE := zeromq-3.2.2
ZMQ_TAR  := $(ZMQ_BASE).tar.gz
ZMQ_URL  := http://download.zeromq.org/$(ZMQ_TAR)

CXX := g++
CXXFLAGS := -I$(LIBZMQ)/include -L$(LIBZMQ)/lib -lzmq -O2 -Wall

all : $(LIBZMQ) $(PROGRAMS)

clean :
	rm $(PROGRAMS)

clean-all : clean
	rm -r $(LIBZMQ)
	rm $(ZMQ_TAR)

rtaild : rtaild.cc
	$(CXX) $(CXXFLAGS) -o $@ rtaild.cc

rtailp : rtailp.cc
	$(CXX) $(CXXFLAGS) -o $@ rtailp.cc

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
