# Please make sure CPLUS_INCLUDE_PATH includes boost's include path and
# LIBRARY_PATH includes boost's library path.

.PHONY: build dist clean fpm

VERSION := 0.5.11
DEB := barn-agent_$(VERSION)_all.deb

default : build

build :
	g++ -pedantic -Wall -std=c++0x -o barn-agent ./src/*.cpp -I./src/external/ -lboost_filesystem -lboost_system -lboost_program_options -lpthread

dist : build $(DEB)

clean :
	rm barn-agent
	rm -rf build
	rm -f *.deb

$(DEB) :
	@echo "Building agent .deb..."
	$(MAKE) fpm
	mkdir -p build/usr/local/bin/
	cp barn-agent build/usr/local/bin
	fpm -C build \
			-s dir -t deb \
			-v $(VERSION) \
			-a all \
			-n barn-agent \
			-d bash \
			-d rsync \
			-d inotify-tools \
			-d ganglia-monitor \
			./usr/local/bin/barn-agent

fpm :
	@echo "Checking for fpm gem..."
	@(gem list | grep ^fpm > /dev/null || gem install fpm)
