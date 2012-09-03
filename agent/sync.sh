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

uname=`uname`                                                                   
if [[ "$uname" == 'Linux' ]]; then                                              
   SLEEP_COMMAND="inotifywait $INOTIFY_EXCLUSIONS --timeout 10 -q -e close_write $RSYNC_LOG_TREE/"
elif [[ "$uname" == 'Darwin' ]]; then                                           
   echo "I'm on OSX so I'm going to loop like crazy. Use Linux for inotify."       
   SLEEP_COMMAND="sleep 10"                                                        
fi

while true; do  # will sleep inside the loop
  for RSYNC_SOURCE in `find $RSYNC_LOG_TREE -type d`; do
    BASE_NAME=$(echo $RSYNC_SOURCE | sed 's/\//_/g')
    HOST_NAME=$(hostname -f)

    echo "Checking for $RSYNC_SOURCE"

    RSYNC_COMMAND_LINE="$RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE/* rsync://$BARN_RSYNC_ADDR/barn_logs/$BASE_NAME@$HOST_NAME/"
    CANDIDATES=$(eval "rsync --dry-run $RSYNC_COMMAND_LINE" | grep -v "created directory" | grep "@" | awk '{print $1}' | sort)
    for c in $CANDIDATES; do
      echo "Candidate is $c"
      rsync $RSYNC_COMMAND_LINE
    done
  done

  if [[ $CANDIDATES == "" ]]; then
    echo "Nothing more to sync. Hibernating till a log file is rotated."
    $SLEEP_COMMAND
  fi

done


