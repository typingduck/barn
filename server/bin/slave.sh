RSYNCD_CONF_NAME="/tmp/barn-slave-rsyncd.conf"

if test "$1" == "" ; then
  echo "Usage: slave.sh RSYNC_PORT" >&2;
  exit 0;
fi

cat >$RSYNCD_CONF_NAME <<EOL
log file = ./tmp/rsyncd-slave.log
pid file = ./tmp/rsyncd-slave.pid
lock file = ./tmp/rsync-slave.lock
port = $RSYNC_PORT

[barn_logs]
   path = ./logs-slave
   read only = false
   comment = Barn Replica Log Server
   use chroot = false
EOL

rsync --verbose --no-detach --daemon --config=./rsyncd-slave.conf
