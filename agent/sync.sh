#!/bin/bash

BARN_CONF="barn.conf"

INOTIFY_EXCLUSIONS="--exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~' --exclude '\.(swx|swp)'"

RSYNC_EXCLUSIONS="--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~ --exclude=\.(swx|swp)"
RSYNC_FLAGS="--verbose"
RSYNC_SOURCE="logs/*"

#while inotifywait $INOTIFY_EXCLUSIONS -q -e close_write logs; do

# No inotify on OSX :(
while true; do
  sleep 1

  echo "START OF RSYNC SESSION"

  CANDIDATES=$(rsync --list-only $RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE $RSYNC_DEST | grep "@" | awk '{print $5}' | sort)

  for c in $CANDIDATES; do
    echo "Candidate is " $c " rotated on  " $(echo $c | tai64nlocal)
    rsync $RSYNC_FLAGS $RSYNC_EXCLUSIONS logs/$c rsync://`cat $BARN_CONF`/barn_logs
  done

done


