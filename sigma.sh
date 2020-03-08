#!/usr/bin/env bash

## the full path of this server
#SERVER_BASE=$(dirname "$(readlink "$0")")
### all valid options
#OPTIONS=(help start stop restart status reload attach kill)
### all valid modes
#MODES=(dev prod test)
### colors
#C_RED="\033[31m"
#C_GREEN="\033[32m"
#C_YELLOW="\033[33m"
#C_BLUE="\033[34m"
#C_DEFAULT="\033[0m"

# show help
show_help() {
  echo "Usage: ./sigma.sh [option] <command>"
  echo "Options:"
  echo "-m "
}

# get the process id of this server
pid() {
  echo 0
}

# show status
show_status() {
  echo "status"
}

# stop server
do_stop() {
  echo "stop"
}

# start server
do_start() {
  echo "start"
}

# restart server
do_restart() {
  echo "restart"
}

# attach to server
do_attach() {
  echo "attach"
}

# reload code
do_reload() {
  echo "reload"
}

# kill the process
do_kill() {
  echo "kill"
}

## main
if [ $# == 0 ]; then
    show_help
    exit 1
fi

SERVER_MODE=${MODES[1]}
while getopts ":m::" opt; do
  case $opt in
  m)
    SERVER_MODE="${OPTARG}"
    ;;
  \?)
    show_help
    ;;
  esac
done

#erl +P $ERL_PROCESSES \
#    +K $POLL \
#    -smp $SMP \
#    -pa ../ebin \
#    -name $NODE_NAME \
#    -setcookie $COOKIE \
#    -boot start_sasl \
#    -config server.config \
#    -kernel error_logger \{file,\""$LOG_PATH"\"\} \
#    -s main server_start
