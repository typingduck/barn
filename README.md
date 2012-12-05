# Barn

Barn is a log aggregation and archival system.

## Overview

Barn attempts to solve the following problems:

* provide high durability guarantees
* provide high throughput
* provide real-time stream inspection
* be easy to integrate with
* be suave to operate
* be smoothly fault-tolerant

The first three requirements are clearly competing with each other. In contrast
to other systems, which attempt to treat log aggregation as a messaging problem,
Barn separates it into two diffent problems: file transfer and messaging.

### Producer

Let's look at the producer: we assume that a process is emitting newline-
separated character data on standard output. Every line is considered a log
record, and gets written to the local disk using the excellent `svlogd` program
from the `runit` package. A separate agent watches `svlogd`s output directory,
and, upon log file rotation, attempts to ship non-current log files to a remote
server.

```shell
# example snippet of a runit 'log/run' script

barn-harvester-agent farm.acme.org:1025 ./main my-service main &
svlogd -tt ./main
```

It is also trivial to intercept the standard output stream using a simple UNIX
pipe and, for example, split it into multiple topic streams, each handled by a
separate `svlogd`:

```shell
# (setup barn-harvester-agent ...)

# capture stdout and split into two streams
tee >( grep "^ERROR" | svlogd -tt ./errors  ) \
    >( grep -v "^ERROR" | svlogd -tt ./main )
    > /dev/null
```

Now, even though we can tweak `svlogd`s rotation policy, we have to wait until a
certain amount of data has been written, until it gets shipped to remote
storage. To get real-time access to the stream, we need to insert `rtail` into
the pipe:

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

### Collector

The destination for `barn-harvester-agent` doesn't do a lot. In fact, the
current implementation is just a `rsync` daemon (so `barn-harvester-agent` is
only little more than a clever way to invoke `rsync`). We also provide some
script to keep a slave `rsync` server in sync (failover of the producers is not
automatic).

### Barn Baler

The collector node is assumed to be just a single machine, thus with limited
disk space. So ultimately we want our data to be archived in distributed storage
-- we use HDFS. Since importing data into HDFS in a reliable, fault-tolerant way
is a bit of a hairy thing, we provide `baler`, which exploits the `Tai64`
timestamps `svlogd` uses to name rotated logfiles to provide idempotent,
block-optimizing writes to HDFS.


## Related Work

* syslog + Flume

This is the combination we've been running in production for about a year. The
main problem was around `append`ing to HDFS files, which is very sensitive to
cluster availablity -- even restarting data nodes could confuse the DFSClient.
We also had to write a lot of custom code (disk buffering, sequence files,
syslog parsing to get the desired bucketing), which would need to be rewritten
for `flume-ng`.

While `syslog` mainly does what you would expect, it is archaic to configure (we
use `rsyslogd`), and doesn't tell you anything if it happens to _not_ do what
you expect.

* AMQP

For a while, we were obsessed with the idea that we could use our existing
RabbitMQ infrastructure and experience, and treat logging as just a very high
throughput messaging problem. But we couldn't get anywhere near the desired
performance.

_TODO: wasn't there some OSS project which uses AMQP?_

* Scribe

Facebook's Scribe essentially implements what `barn-agent` does. It does it
well, or so we hear, but we would still have had to implement the other parts
ourselves.  Another issue is that Scribe speaks Thrift, over TCP, exclusively.
Which, apart from more involved integration in heterogenous environments, would
require the `Haystacker` to parse, inspect and transform log files -- we can't
and don't want to use Thrift for long-term archival, and tailor all our
workflows around that.

* Kafka

LinkedIn's Kafka is a very interesting project, so we looked at it very hard.
And from different angles. And with 1-6 eyes. Eventually, we decided to not use
it, because it seemed too much of an operational overhead to keep it running
reliably and with high throughput. Namely:

  * Kafka relies heavily on ZooKeeper, which is hard to operate
  * as we understand, a single Kafka broker shouldn't serve a large number of
    producers: due to the non-reliable messaging protocol Kafka implements, if a
    broker goes down, all messages currently in-flight will be lost (most likely
    more, depending on the client's network implementation). This might be fine,
    eg. if you're anyway planning to dedicate a machine per rack to collect and
    forward logs, and if you can afford to loose a couple of messages here and
    there. In our setup, however, it isn't.
  * Kafka uses a custom binary wire protocol, which is also used for disk
    persistence. That's nice, but we again would have to parse it before writing
    to HDFS.

That being said, `barn`s design is heavily influenced by Kafka, except that we
got rid of coordination (mostly) and that we push the "queue history" idea to
the producer itself (as well as the collector).

* Others

There are also a couple of other systems out there, eg. Heroku's logplex, or
logstash. They seem to be targeted at very different scenarios though, and
wouldn't satisfy all of our requirements. YMMV, as usual.


## Build

Make sure you have the following installed on the build system:

* a recent gcc
* a recent JDK
* a recent Haskell Platform
* a working `make`

Then, in the top level of the repository, simply type:

```shell
make dist
```

## Installation

We provide debian packages of all `barn` components (_TODO_), as well has zip
and tar packages. The runtime dependencies are as follows:

### Agent

runit, rsync, inotify, bash, (grep, awk), optional: rtail

### Collector

rsync, bash

### Haystacker

JRE >= 6, and a HDFS cluster to talk to


We recommend to run the `Collector` and `Haystacker` as `runit` services.


## Configuration

All `barn` executables are configured through command line parameters. Where
`svlogd` is used, refer to `man svlogd` for additional file based configuration
options.

Some knobs to turn are:

### Agent

* you may want to adjust the rotation / history policy for `svlogd` to the
  amount of disk space you have available
* for low-volume producers, you may want to set the rotation policy for `svlogd`
  to a low value
* for high-volume producers, you may want to mount a RAM disk for `svlogd` to
  write to. This will allow the producer to continue functioning even if the
  machine's disk crashes, giving up a little bit of safety in case the system
  needs to be power-cycled.
* a RAM disk may also be an option if you can't write to local disk at all
* _TODO_ rtail uses a 0mq PUB socket over TCP to publish log messages. This is
  because the assumption is that most of the time, data will be discarded
  anyway, and there are only a few consumers on only a few services at any time.
  As the number of consumers grows, this may cause network congestion because
  the log streams are duplicated many times. We'll look into providing an option
  to use UDP (PGM) instead for those scenarios (PGM is not available on all
  platforms, and would cause constant network traffic _by default_).

### Collector / Haystacker

It's all about RAID and disk partitioning. Heavily depends on what you want to
optimize for. Make sure you can use as much bandwidth as possible to talk to
HDFS (possibly use two NICs on two switches for ingress and egress
respectively).

### Failover

At this point, we don't have any automatic failover mechanism for `barn-agent`
in case the collector is not reachable. We recommend to use a DNS entry with a
short TTL and perform manual failover.


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


## Maintainers

  * Omid Aladini <omid@soundcloud.com>
  * Kim Altintop <kim@soundcloud.com>

## Contributors

  * Brendan Hay <brendan@soundcloud.com>
  * Torsten Curdt <tcurdt@soundcloud.com>
