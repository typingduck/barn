.PHONY: all clean

COMPONENTS := agent rtail server testing

all :
	for c in $(COMPONENTS); do (cd $$c; make); done

clean :
	for c in $(COMPONENTS); do (cd $$c; make clean); done
