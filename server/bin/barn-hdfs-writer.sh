if test "$3" == "" ; then
  echo "Usage: barn-hdfs-writer HADOOP_ADDR LOGS_ROOT HDFS_LOGS_ROOT" >&2;
  echo " " >&2;
  echo "HADOOP_ADDR : your namenode address including port." >&2;
  echo "LOGS_ROOT : local logs root directory." >&2;
  echo "HDFS_LOGS_ROOT : hdfs logs root directory." >&2;
  echo " -- Please set CLASSPATH env variable properly." >&2;
  exit 1;
fi

HADOOP_ADDR=$1
LOGS_ROOT=$2
HDFS_LOGS_ROOT=$3

java barn.BarnHdfsWriter -fs $HADOOP_ADDR $LOGS_ROOT $HDFS_LOGS_ROOT
