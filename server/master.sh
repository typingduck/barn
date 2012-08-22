RSYNCD_CONF_NAME="/tmp/barn-master-rsyncd.conf"

if test "$1" == "" ; then
  echo "Usage: master.sh RSYNC_PORT" >&2;
  exit 0;
fi

RSYNC_PORT=$1

cat >$RSYNCD_CONF_NAME <<EOL
log file = ./sys/rsyncd.log
pid file = ./sys/rsyncd.pid
lock file = ./sys/rsync.lock
port = $RSYNC_PORT

[barn_logs]
   path = ./logs
   read only = false
   comment = Barn Log Server
   use chroot = false
EOL

rsync --verbose --no-detach --daemon --config=$RSYNCD_CONF_NAME
