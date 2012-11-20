.PHONY: all clean deps test fpm start-hadoop stop-hadoop

AGENT_VERSION := 0.5.0
AGENT_DEB := barn-agent_$(AGENT_VERSION)_all.deb

SERVER_VERSION := $(shell cat server/build.sbt | sed -n -E -e 's/version[[:space:]]+:=[[:space:]]+"([^"]+)"/\1/p')
SERVER_JAR := server/target/barn-hdfs-writer-assembly-$(SERVER_VERSION).jar
SERVER_ZIP := server/dist/barn-server-$(SERVER_VERSION).zip

LOG_GEN := test/svlogd-log-generator/loggen

DAHOOP_BASE := hadoop-0.20.2-cdh3u5
DAHOOP_HOME := test/$(DAHOOP_BASE)
DAHOOP_URL  := http://archive.cloudera.com/cdh/3/$(DAHOOP_BASE).tar.gz

all : deps $(AGENT_DEB) $(SERVER_ZIP)

deps : fpm $(LOG_GEN) $(DAHOOP_HOME)

test : start-hadoop
	(cd server; sbt test)
	make stop-hadoop

clean :
	rm -r build
	rm *.deb
	(cd server; sbt clean)

start-hadoop : $(DAHOOP_HOME)
	(cd $(DAHOOP_HOME); ./bin/hadoop namenode -format; ./bin/start-all.sh)

stop-hadoop : $(DAHOOP_HOME)
	(cd $(DAHOOP_HOME); ./bin/stop-all.sh)

$(AGENT_DEB) :
	mkdir -p build/agent/usr/local/bin/
	cp agent/barn-agent build/agent/usr/local/bin
	fpm -C build/agent \
			-s dir -t deb \
			-v $(AGENT_VERSION) \
			-a all \
			-n barn-agent \
			-d bash \
			-d rsync \
			-d inotify-tools \
			./usr/local/bin/barn-agent

$(SERVER_ZIP) :
	(cd server; sbt package-dist)

fpm :
	gem list | grep ^fpm || gem install fpm

$(LOG_GEN) :
	(cd test/svlogd-log-generator; g++ -o loggen *.cpp)

$(DAHOOP_HOME) :
	make $(DAHOOP_HOME).tar.gz
	(cd test; \
		tar xvfz $(DAHOOP_BASE).tar.gz; \
		cp conf-hadoop-pseudo/*.xml $(DAHOOP_BASE)/conf)


$(DAHOOP_HOME).tar.gz :
	curl $(DAHOOP_URL) > $(DAHOOP_HOME).tar.gz
