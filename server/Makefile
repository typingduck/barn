.PHONY: all clean test start-hadoop stop-hadoop

VERSION := $(shell cat build.sbt | sed -n -E -e 's/version[[:space:]]+:=[[:space:]]+"([^"]+)"/\1/p')
PACKAGE_ZIP := dist/barn-server-$(VERSION).zip

DAHOOP_BASE := hadoop-0.20.2-cdh3u5
DAHOOP_URL  := http://archive.cloudera.com/cdh/3/$(DAHOOP_BASE).tar.gz

all : $(PACKAGE_ZIP)

test : start-hadoop
	sbt test
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

$(PACKAGE_ZIP) :
	@echo "Building server .zip..."
	@make start-hadoop
	(cd server; sbt package-dist)
	@make stop-hadoop

$(DAHOOP_BASE) :
	make $(DAHOOP_BASE).tar.gz
	tar xvfz $(DAHOOP_BASE).tar.gz && \
	cp conf-hadoop-pseudo/*.xml $(DAHOOP_BASE)/conf


$(DAHOOP_BASE).tar.gz :
	curl $(DAHOOP_URL) > $(DAHOOP_BASE).tar.gz
