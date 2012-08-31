RSYNCD_CONF_NAME="/tmp/barn-master-rsyncd.conf"

if test "$1" == "" ; then
  echo "Usage: master.sh RSYNC_PORT" >&2;
  exit 0;
fi

RSYNC_PORT=$1

cat >$RSYNCD_CONF_NAME <<EOL
log file = /tmp/rsyncd.log
pid file = /tmp/rsyncd.pid
lock file = /tmp/rsync.lock
port = $RSYNC_PORT

[barn_logs]
   path = ./logs
   read only = false
   comment = Barn Log Server
   use chroot = false
EOL

rsync --verbose --no-detach --daemon --config=$RSYNCD_CONF_NAME
