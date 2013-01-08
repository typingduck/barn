.PHONY: build clean dist test test-thorough start-hadoop stop-hadoop

VERSION := $(shell cat build.sbt | sed -n -E -e 's/version[[:space:]]+:=[[:space:]]+"([^"]+)"/\1/p')
PACKAGE_ZIP := dist/barn-baler-$(VERSION).zip
SRC := src/main/scala/**/*.scala

DAHOOP_BASE := hadoop-0.20.2-cdh3u5
DAHOOP_URL  := http://archive.cloudera.com/cdh/3/$(DAHOOP_BASE).tar.gz
DAHOOP_DATA := hadoop-data

SBT 		:= sbt/bin/sbt
SBT_URL := http://scalasbt.artifactoryonline.com/scalasbt/sbt-native-packages/org/scala-sbt/sbt/0.12.1/sbt.tgz

default : build

build : $(SBT) $(SRC)
	$(SBT) compile

dist : $(PACKAGE_ZIP)

test :
	$(SBT) test

test-thorough : start-hadoop
	$(SBT) test slow:test
	$(MAKE) stop-hadoop

clean :
	rm -rf dist
	$(SBT) clean

start-hadoop : $(DAHOOP_BASE) $(DAHOOP_DATA)
	@echo "Starting Dahoops..."
	(cd $(DAHOOP_BASE); \
	 ./bin/hadoop namenode& echo $$! > namenode.pid; \
	 ./bin/hadoop datanode& echo $$! > datanode.pid)

stop-hadoop : $(DAHOOP_BASE)
	@echo "Stopping Dahoops..."
	(cd $(DAHOOP_BASE); \
	 kill `cat namenode.pid`; \
	 kill `cat datanode.pid`; \
	 rm *.pid)

$(PACKAGE_ZIP) : $(SRC)
	@echo "Building barn-baler .zip..."
	$(MAKE) start-hadoop
	$(SBT) clean slow:test package-dist
	$(MAKE) stop-hadoop

$(DAHOOP_BASE) :
	$(MAKE) $(DAHOOP_BASE).tar.gz
	tar xvfz $(DAHOOP_BASE).tar.gz && \
	cp conf-hadoop-pseudo/*.xml $(DAHOOP_BASE)/conf


$(DAHOOP_BASE).tar.gz :
	curl $(DAHOOP_URL) > $(DAHOOP_BASE).tar.gz

$(DAHOOP_DATA) :
	mkdir $(DAHOOP_DATA) $(DAHOOP_DATA)/name
	(cd $(DAHOOP_BASE); echo "Y" | ./bin/hadoop namenode -format)

$(SBT) :
	curl $(SBT_URL) > sbt.tgz && \
	tar xvfz sbt.tgz && \
	rm sbt.tgz
