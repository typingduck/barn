#!/bin/bash

if [[ "$1" == "" || "$2" == "" ]]; then
  echo "Usage: sync.sh RSYNC_HOST:RSYNC_PORT RSYNC_LOG_TREE";
  echo "   RSYNC_LOG_TREE could be a ls-compatible glob"
  exit 0;
fi

BARN_RSYNC_ADDR=$1

INOTIFY_EXCLUSIONS="--exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~'"

RSYNC_EXCLUSIONS="--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~"
RSYNC_FLAGS="-c --verbose"  # --verbose is important since we use it to issue rsync incrementally
RSYNC_LOG_TREE="$2"

#while inotifywait $INOTIFY_EXCLUSIONS -q -e close_write logs; do

# No inotify on OSX :(
while true; do
  for RSYNC_SOURCE in `ls "$RSYNC_LOG_TREE"`; do
    BASE_NAME=$(basename $RSYNC_SOURCE)
    HOST_NAME=$(hostname -f)

    echo "Checking for $RSYNC_LOG_TREE$RSYNC_SOURCE.. "

    RSYNC_COMMAND_LINE="$RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_LOG_TREE$RSYNC_SOURCE/* rsync://$BARN_RSYNC_ADDR/barn_logs/$BASE_NAME@$HOST_NAME/"
    CANDIDATES=$(eval "rsync --dry-run $RSYNC_COMMAND_LINE" | grep "@" | awk '{print $1}' | sort)
    for c in $CANDIDATES; do
      echo "Candidate is $c"
      rsync $RSYNC_COMMAND_LINE
    done
  done
done


