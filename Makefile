.PHONY: build clean dist

COMPONENTS := agent rtail server testing

default : build

build :
	for c in $(COMPONENTS); do (cd $$c; make build); done

clean :
	for c in $(COMPONENTS); do (cd $$c; make clean); done

dist :
	for c in $(COMPONENTS); do (cd $$c; make dist); done
