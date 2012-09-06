#!/bin/bash

if [[ "$3" == "" ]]; then
  echo "Usage: sync.sh RSYNC_HOST:RSYNC_PORT RSYNC_LOG_TREE SERVICE_NAME_SED_EXPRESSION";
  echo "   RSYNC_LOG_TREE could be a ls-compatible glob"
  exit 0;
fi

BARN_RSYNC_ADDR=$1
INOTIFY_EXCLUSIONS="--exclude '\.u' --exclude 'lock' --exclude 'current' --exclude '*~'"
RSYNC_EXCLUSIONS="--exclude=*.u --exclude=config --exclude=current --exclude=lock --exclude=*~"
RSYNC_FLAGS="-c --verbose"  # --verbose is important since we use it to issue rsync incrementally
RSYNC_LOG_TREE=$2
RSYNC_LOG_TREE_EXCLUDE="supervise" # Runit specific # FIXME

# For exmample for sv: 's/\/etc\/sv\/\(.*\)\/log\/\(.*\)/\1@\2/'
# Extracts a name from paths like /etc/sv/*/log/*
# Or for any basename match: 's#.*/##'
SERVICE_NAME_SED_EXPRESSION=$3

function killtree {
    local _pid=$1
    local _sig=${2-TERM}
    for _child in $(ps -o pid --no-headers --ppid ${_pid}); do
        killtree ${_child} ${_sig}
    done
    kill -${_sig} ${_pid} > /dev/null 2>&1
}

function close {
  echo "Sending $1 to all children. Say goodbye kids!"
  killtree $$ $1
  sleep 1
  exit 0
}

#Monitor a subdirectory
function sleepit {
  uname=`uname`
  RSYNC_SOURCE=$1
  echo "Nothing more to sync. Hibernating till a log file is rotated on $RSYNC_SOURCE"
  if [[ "$uname" == 'Linux' ]]; then
     inotifywait $INOTIFY_EXCLUSIONS --timeout 3600 -q -e close_write $RSYNC_SOURCE/
  elif [[ "$uname" == 'Darwin' ]]; then
     echo "I'm on OSX so I'm going to loop-sleep like a madman. Use Linux for inotify."
     sleep 10
  fi
}

# Take one argument and sync it to the target barn rsync server
function sync {
  RSYNC_SOURCE=$1
  SERVICE_NAME=$(echo $RSYNC_SOURCE | sed $SERVICE_NAME_SED_EXPRESSION)
  HOST_NAME=$(hostname -f)

  echo "Checking for $SERVICE_NAME"

  RSYNC_COMMAND_LINE="$RSYNC_FLAGS $RSYNC_EXCLUSIONS $RSYNC_SOURCE/* rsync://$BARN_RSYNC_ADDR/barn_logs/$SERVICE_NAME@$HOST_NAME/"
  CANDIDATES=$(eval "rsync --dry-run $RSYNC_COMMAND_LINE" | grep -v "created directory" | grep "@" | awk '{print $1}' | sort)
  for c in $CANDIDATES; do
    echo "Candidate on $RSYNC_SOURCE is $c"
    rsync $RSYNC_COMMAND_LINE
  done

  if [[ $CANDIDATES == "" ]]; then
    return 1   #I didn't sync anything
  else
    return 0
  fi
}

# Main program is here
for RSYNC_SOURCE in `find $RSYNC_LOG_TREE -type d -and -not -name $RSYNC_LOG_TREE_EXCLUDE`; do
  (
    while true; do
      sync $RSYNC_SOURCE || sleepit $RSYNC_SOURCE
    done
  ) &
done

trap 'close TERM' SIGTERM
trap 'close INT' SIGINT

wait

