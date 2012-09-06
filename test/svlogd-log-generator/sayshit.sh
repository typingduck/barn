#!/bin/bash

if test "$1" == "" ; then
  echo "Usage: sayshit.sh SIZE_IN_MB_PER_SEC" >&2;
  exit 0;
fi

DIR="$( cd "$( dirname "$0" )" && pwd )"

size_in_mb=$1

while true; do
  $DIR/loggen $size_in_mb
  sleep 1     # We should ( 1 - time_it_took_to_generate_logs ) instead
done
