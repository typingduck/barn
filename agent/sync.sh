#!/bin/bash

BARN_CONF="barn.conf"

while inotifywait --exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~' --exclude '\.(swx|swp)' -q -e close_write logs; do
  rsync --size-only --verbose --exclude '*u' --exclude 'config' --exclude 'current' --exclude 'lock' --exclude '*~' --exclude '\.(swx|swp)' logs/* rsync://`cat $BARN_CONF`/barn_logs | tee rsync.log
  for i in $(cat rsync.log | grep @); do rm logs/$i; done
done


