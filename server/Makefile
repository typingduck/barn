.PHONY: build clean dist test test-thorough start-hadoop stop-hadoop

VERSION := $(shell cat build.sbt | sed -n -E -e 's/version[[:space:]]+:=[[:space:]]+"([^"]+)"/\1/p')
PACKAGE_ZIP := dist/barn-server-$(VERSION).zip
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
	@make stop-hadoop

clean :
	rm -r dist
	sbt clean

start-hadoop : $(DAHOOP_BASE)
	@echo "Starting Dahoops..."
	(cd $(DAHOOP_BASE); ./bin/hadoop namenode -force -format; ./bin/start-all.sh)

stop-hadoop : $(DAHOOP_BASE)
	@echo "Stopping Dahoops..."
	(cd $(DAHOOP_BASE); ./bin/stop-all.sh)

$(PACKAGE_ZIP) : $(SRC)
	@echo "Building server .zip..."
	@make start-hadoop
	sbt clean slow:test package-dist
	@make stop-hadoop

$(DAHOOP_BASE) :
	make $(DAHOOP_BASE).tar.gz
	tar xvfz $(DAHOOP_BASE).tar.gz && \
	cp conf-hadoop-pseudo/*.xml $(DAHOOP_BASE)/conf


$(DAHOOP_BASE).tar.gz :
	curl $(DAHOOP_URL) > $(DAHOOP_BASE).tar.gz
