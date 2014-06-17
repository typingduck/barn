# RTail

RTail can be used to get realtime access to the log stream by inserting it into the pipe:

```shell
# (setup barn-harvester-agent ...)

RTAILD_SOCK=./rtaild.sock
RTAILD_PORT=12345

rtaild $RTAILD_SOCK $RTAILD_PORT &

tee >( grep "^ERROR" | rtailp $RTAILD_SOCK myservice.errors.$(hostname) | svlogd -tt ./errors ) \
    >( grep -v "^ERROR" | rtailp $RTAILD_SOCK myservice.main.$(hostname) | svlogd -tt ./main )
    > /dev/null
```

We can now use the `rtail` program to connect to a set of machines, and
subscribe to (a subset of) the log streams produced by services running there:

```shell
HOSTS="host1:12345 host2:12345 host3:12345"
# subscribe to all streams of 'myservice' on three machines:
rtail $HOSTS myservice

# subscribe to just the 'error' stream:
rtail $HOSTS myservice.error

# subscribe to the 'main' stream, and the 'error' stream of host3:
rtail $HOSTS myservice.main myservice.error.host3
```
## Build Requirements
* a recent Haskell Platform
* libzmq is included

## FAQ

* **Why is rtail written in Haskell?! I thought that's for academic curmudgeons
  who wear no shoes, even in winter!**

  We _are_ curmudgeons, appreciate academia (from a safe distance), and we wear
  shoes (most of the time).

  There's also a C++ implementation (see the rtail++ branch), but it needs some
  love: rtailp currently consumes way too much CPU.

* **Why is rtail not written in Go?! I hear it's webscale!**

  Yeah sure. We'll consider it when it's implementation reaches version 2.0, and
  someone writes a proper 0mq binding.


## TODO rtail uses a 0mq PUB socket over TCP to publish log messages. This is
  because the assumption is that most of the time, data will be discarded
  anyway, and there are only a few consumers on only a few services at any time.
  As the number of consumers grows, this may cause network congestion because
  the log streams are duplicated many times. We'll look into providing an option
  to use UDP (PGM) instead for those scenarios (PGM is not available on all
  platforms, and would cause constant network traffic _by default_).
