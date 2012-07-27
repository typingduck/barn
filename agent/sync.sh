#!/bin/bash

BARN_CONF="barn.conf"

INOTIFY_EXCLUSIONS="--exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~' --exclude '\.(swx|swp)'"

RSYNC_EXCLUSIONS="--exclude '*u' --exclude 'config' --exclude 'current' --exclude 'lock' --exclude '*~' --exclude '\.(swx|swp)'"
RSYNC_FLAGS="--size-only --verbose"
RSYNC_SOURCE="logs/*"

while inotifywait $INOTIFY_EXCLUSIONS -q -e close_write logs; do
  CANDIDATES=$(rsync --list-only $RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE $RSYNC_DEST | grep "@" | awk '{print $5}' | sort)
  for c in $CANDIDATES; do
    echo "Candidate is " + $c
    rsync $RSYNC_FLAGS $RSYNC_EXCLUSIONS logs/$c rsync://`cat $BARN_CONF`/barn_logs | tee rsync.log
  done
  #rsync $RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE rsync://`cat $BARN_CONF`/barn_logs | tee rsync.log
  for i in $(cat rsync.log | grep @); do rm logs/$i; done
done


