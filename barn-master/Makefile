.PHONY: build dist clean fpm

VERSION := 0.5.6
DEB := barn-master_$(VERSION)_all.deb

default : dist

build : default

dist : $(DEB)

clean :
	rm -rf build
	rm -f *.deb

$(DEB) :
	@echo "Building master .deb..."
	$(MAKE) fpm
	mkdir -p build/usr/local/bin/
	cp barn-* build/usr/local/bin
	fpm -C build \
			-s dir -t deb \
			-v $(VERSION) \
			-a all \
			-n barn-master \
			-d bash \
			-d rsync \
			-d inotify-tools \
			./usr/local/bin/barn-master

fpm :
	@echo "Checking for fpm gem..."
	@(gem list | grep ^fpm > /dev/null || gem install fpm)
