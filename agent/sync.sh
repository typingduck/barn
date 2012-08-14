#!/bin/bash

BARN_CONF="barn.conf"

INOTIFY_EXCLUSIONS="--exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~'"

RSYNC_EXCLUSIONS="--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~"
RSYNC_FLAGS="-c --verbose"
RSYNC_SOURCE_TREE="logs/*"

#while inotifywait $INOTIFY_EXCLUSIONS -q -e close_write logs; do

# No inotify on OSX :(
while true; do
  for RSYNC_SOURCE in $RSYNC_SOURCE_TREE; do
    BASE_NAME=$(basename $RSYNC_SOURCE)
    CANDIDATES=$(rsync --dry-run $RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE/* rsync://`cat $BARN_CONF`/barn_logs/$BASE_NAME/ | grep "@" | awk '{print $1}' | sort)
    echo "CANDIDATES for $RSYNC_SOURCE: $CANDIDATES"
    for c in $CANDIDATES; do
      echo "Candidate is $c"
      rsync $RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE/$c rsync://`cat $BARN_CONF`/barn_logs/$BASE_NAME/
    done
  done
done


