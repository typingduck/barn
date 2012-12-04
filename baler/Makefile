.PHONY: build clean dist test test-thorough start-hadoop stop-hadoop

VERSION := $(shell cat build.sbt | sed -n -E -e 's/version[[:space:]]+:=[[:space:]]+"([^"]+)"/\1/p')
PACKAGE_ZIP := dist/barn-baler-$(VERSION).zip
SRC := src/main/scala/**/*.scala

DAHOOP_BASE := hadoop-0.20.2-cdh3u5
DAHOOP_URL  := http://archive.cloudera.com/cdh/3/$(DAHOOP_BASE).tar.gz

default : build

build : $(SRC)
	sbt compile

dist : $(PACKAGE_ZIP)

test :
	sbt test

test-thorough : start-hadoop
	sbt test slow:test
	$(MAKE) stop-hadoop

clean :
	rm -r dist
	sbt clean

start-hadoop : $(DAHOOP_BASE)
	@echo "Starting Dahoops..."
	(cd $(DAHOOP_BASE); \
	 echo "Y" | ./bin/hadoop namenode -format; \
	 ./bin/start-all.sh)

stop-hadoop : $(DAHOOP_BASE)
	@echo "Stopping Dahoops..."
	(cd $(DAHOOP_BASE); ./bin/stop-all.sh)

$(PACKAGE_ZIP) : $(SRC)
	@echo "Building barn-baler .zip..."
	$(MAKE) start-hadoop
	sbt clean slow:test package-dist
	$(MAKE) stop-hadoop

$(DAHOOP_BASE) :
	$(MAKE) $(DAHOOP_BASE).tar.gz
	tar xvfz $(DAHOOP_BASE).tar.gz && \
	cp conf-hadoop-pseudo/*.xml $(DAHOOP_BASE)/conf


$(DAHOOP_BASE).tar.gz :
	curl $(DAHOOP_URL) > $(DAHOOP_BASE).tar.gz
