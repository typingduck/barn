#!/bin/bash

if test "$1" == "" ; then
  echo "Usage: svlogd.sh ROOT_LOG_DIRECTORY SERVICE_NAME" >&2;
  exit 0;
fi

LOG_DIRECTORY=$1$2

cat >$LOG_DIRECTORY/config <<EOL
#Dynamically generated, your changes will be overwritten and ignored
s1000000
n100
t3600
EOL

mkdir -p $LOG_DIRECTORY
svlogd -v -tt $LOG_DIRECTORY
