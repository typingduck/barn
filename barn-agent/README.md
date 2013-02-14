Barn Agent
--------

Barn Agent, runs as a part of Barn logging pipeline. It monitors directories containing rotating log files and upon rotation of a new log file, it ships the file to the Barn Master. 

Barn Agent is written in C++ and requires the following dependencies:

* Compile-time:
  - Boost Libraries (tested against 1.53.0) and needs to link against (boost::system, boost::filesystem)
  - Boost.Process (which is not a part of official boost libraries, but included in the source three.)
  - A C++0x compliant to compile C++0x features. Tested with GCC 4.7.2.
* Run-time:
  - Rsync (tested on 3.0.7 but should work with 3.0.x)
  - inotifywait (which I'll remove the dependency in the future by replacing with system calls)


#### How to build

* Build boost
* Run make specifying where boost headers and libraries reside:

     ```
     $ CPLUS_INCLUDE_PATH=<PATH_TO_BOOST_HEADERS> LIBRARY_PATH=<PATH_TO_BOOST_LIBRARIES> make
      ```
      
* You can also make a debian package out of the binary, via ```make dist```.


#### How to run

```
./barn-agent BARN_MASTER_HOST:PORT LOCAL_LOG_DIRECTORY NAME_OF_SERVICE CATEGORY
```

####### Barn Agent is a part of the Barn package and Barn's LICENSE applies.
