.PHONY: build clean

COMPONENTS := agent rtail server testing

default : build

build :
	for c in $(COMPONENTS); do (cd $$c; make); done

clean :
	for c in $(COMPONENTS); do (cd $$c; make clean); done
