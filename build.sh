#!/bin/bash

if [[ "$1" == "" ]]; then
  echo "Usage: $0 VERSION" >&2
  exit 1;
fi

command -v fpm > /dev/null
FPM_EXISTS=$?

if [[ $FPM_EXISTS != 0 ]]; then
  echo "ERROR: This build script depends on fpm gem. Please make it available on the path" >&2
  exit 1;
fi

VERSION=$1
TEMP_DIR=`mktemp -d -t barnbuild.XXXXXX`
CUR_DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p $TEMP_DIR/usr/local/bin/
cp $CUR_DIR/agent/barn-agent $TEMP_DIR/usr/local/bin

fpm -C $TEMP_DIR \
    -s dir -t deb \
    -v $VERSION \
    -a all \
    -n barn-agent \
    -d bash \
    -d rsync \
    -d inotify-tools \
    ./usr/local/bin/barn-agent

rm -rf $TEMP_DIR
