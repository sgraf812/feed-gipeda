#!/bin/bash
### BEGIN INIT INFO
# Provides:          feed-gipeda-master
# Required-Start:    $remote_fs $network $named $time $syslog
# Required-Stop:     $remote_fs $network $named $time $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Description:       Starts a master node of feed-gipeda in hourly watch mode, deploying to /var/www/html/, the default nginx location.
### END INIT INFO


SCRIPT="mkdir -p /root/feed-gipeda-artifacts && cd /root/feed-gipeda-artifacts && PATH=\$PATH:/root/.local/bin feed-gipeda --master=localhost:1337 --deploy-to=/var/www/html/ --watch=3600"
RUNAS=root

PGIDFILE=/var/run/feed-gipeda-master.pgid
LOGFILE=/var/log/feed-gipeda-master.log

start() {
  if [ -f /var/run/$PIDNAME ] && kill -0 $(cat /var/run/$PIDNAME); then
    echo 'Service already running' >&2
    return 1
  fi
  echo 'Starting service…' >&2
  local CMD="($SCRIPT) &> \"$LOGFILE\" & echo \$!"
  local PID=$(su -c "$CMD" $RUNAS)
  echo $PID
  ps -p $PID -o "%r" --no-header | xargs > "$PGIDFILE" # xargs for trimming
  echo 'Service started' >&2
}

stop() {
  if [ ! -f "$PGIDFILE" ] || ! kill -0 -- -$(cat "$PGIDFILE"); then
    echo 'Service not running' >&2
    return 1
  fi
  echo 'Stopping service…' >&2
  kill -- -$(cat "$PGIDFILE") && rm -f "$PGIDFILE"
  echo 'Service stopped' >&2
}

uninstall() {
  echo -n "Are you sure you want to uninstall this service? That cannot be undone. [yes|No] "
  local SURE
  read SURE
  if [ "$SURE" = "yes" ]; then
    stop
    rm -f "$PGIDFILE"
    echo "Notice: log file will not be removed: '$LOGFILE'" >&2
    update-rc.d -f feed-gipeda-master remove
    rm -fv "$0"
  fi
}

case "$1" in
  start)
    start
    ;;
  stop)
    stop
    ;;
  uninstall)
    uninstall
    ;;
  restart)
    stop
    start
    ;;
  *)
    echo "Usage: $0 {start|stop|restart|uninstall}"
esac
