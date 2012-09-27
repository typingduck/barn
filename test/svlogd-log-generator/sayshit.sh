#!/bin/bash

if test "$1" == "" ; then
  echo "Usage: sayshit.sh SIZE_IN_MB_PER_SEC" >&2;
  exit 0;
fi

size_in_mb=$1
wd=$(dirname $0)

while true; do
  $wd/loggen $size_in_mb
  sleep 1     # We should ( 1 - time_it_took_to_generate_logs ) instead
done
