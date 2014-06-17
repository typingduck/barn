.PHONY: build clean dist

COMPONENTS := rtail

default : build

build :
	for c in $(COMPONENTS); do (cd $$c; $(MAKE) build); done

clean :
	for c in $(COMPONENTS); do (cd $$c; $(MAKE) clean); done

dist :
	for c in $(COMPONENTS); do (cd $$c; $(MAKE) dist); done
