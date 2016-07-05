#!/bin/bash
### BEGIN INIT INFO
# Provides:          feed-gipeda-slave
# Required-Start:    $remote_fs $network $named $time $syslog
# Required-Stop:     $remote_fs $network $named $time $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Description:       Starts a slave node for feed-gipeda, awaiting benchmark jobs from a master node.
### END INIT INFO


SCRIPT="PATH=$PATH:/root/.local/bin feed-gipeda --slave=localhost:1338"
RUNAS=root

PGIDFILE=/var/run/feed-gipeda-slave.gpid
LOGFILE=/var/log/feed-gipeda-slave.log

start() {
  if [ -f /var/run/$PIDNAME ] && kill -0 $(cat /var/run/$PIDNAME); then
    echo 'Service already running' >&2
    return 1
  fi
  echo 'Starting service…' >&2
  local CMD="$SCRIPT &> \"$LOGFILE\" & echo \$!"
  local PID=$(su -c "$CMD" $RUNAS)
  ps -p $PID -o "%r" --no-header > "$PGIDFILE"
  echo 'Service started' >&2
}

stop() {
  if [ ! -f "$PGIDFILE" ] || ! kill -0 -- -$(cat "$PGIDFILE"); then
    echo 'Service not running' >&2
    return 1
  fi
  echo 'Stopping service…' >&2
  kill -TERM -- -$(cat "$PGIDFILE") && rm -f "$PGIDFILE"
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
    update-rc.d -f feed-gipeda-slave remove
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

