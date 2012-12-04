.PHONY: build clean

LOG_GEN := svlogd-log-generator

build : $(LOG_GEN)/loggen

$(LOG_GEN)/loggen : $(LOG_GEN)/*.cpp
	(cd svlogd-log-generator; g++ -o loggen *.cpp)

clean :
	rm $(LOG_GEN)/loggen
