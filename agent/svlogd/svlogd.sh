#!/bin/bash

if test "$1" == "" ; then
  echo "Usage: svlogd.sh ROOT_LOG_DIRECTORY SERVICE_NAME" >&2;
  exit 0;
fi

LOG_DIRECTORY=$1$2

mkdir -p $LOG_DIRECTORY
svlogd -v -tt $LOG_DIRECTORY
