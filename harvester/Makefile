.PHONY: build dist clean fpm

VERSION := 0.5.1
DEB := barn-harvester_$(VERSION)_all.deb

default : dist

build : default

dist : $(DEB)

clean :
	rm -r build
	rm *.deb

$(DEB) :
	@echo "Building harvester .deb..."
	@make fpm
	mkdir -p build/usr/local/bin/
	cp barn-* build/usr/local/bin
	fpm -C build \
			-s dir -t deb \
			-v $(VERSION) \
			-a all \
			-n barn-harvester \
			-d bash \
			-d rsync \
			-d inotify-tools \
			./usr/local/bin/barn-harvester-agent \
			./usr/local/bin/barn-harvester-master \
			./usr/local/bin/barn-harvester-replicate \
			./usr/local/bin/barn-harvester-slave

fpm :
	@echo "Checking for fpm gem..."
	@(gem list | grep ^fpm > /dev/null || gem install fpm)
