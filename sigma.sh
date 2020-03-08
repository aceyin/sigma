#!/usr/bin/env bash

# the full path of this server
SERVER_BASE=$(dirname "$(readlink -f "$0")")



NODE=3
COOKIE=sigma_server
NODE_NAME=$COOKIE.$NODE@127.0.0.1

#ulimit -SHn 102400

# define default configuration
POLL=true
SMP=auto
ERL_MAX_PORTS=32000
ERL_PROCESSES=500000
ERL_MAX_ETS_TABLES=1400

export ERL_MAX_PORTS
export ERL_MAX_ETS_TABLES
export SERVER_BASE

DATETIME=$(date "+%Y%m%d%H%M%S")
LOG_PATH="../logs/app_$NODE.$DATETIME.log"

erl +P $ERL_PROCESSES \
    +K $POLL \
    -smp $SMP \
    -pa ../ebin \
    -name $NODE_NAME \
    -setcookie $COOKIE \
    -boot start_sasl \
    -config server.config \
    -kernel error_logger \{file,\""$LOG_PATH"\"\} \
    -s main server_start