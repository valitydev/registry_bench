#!/bin/bash

ALL_NODES_COUNT=$1
PROC_COUNT=$2
WITH_CONFLICT=$3
HOST=`hostname -f`
RELEASE_LAUNCHER="_build/default/rel/registry_bench/bin/registry_bench"

#if [ ! -f $RELEASE_LAUNCHER ]; then
#  rebar3 release
#fi

rebar3 release
echo
for ((i=1; i <= ALL_NODES_COUNT ; i++)); do
  echo "$i node starting"
  if [ $i -ne ${ALL_NODES_COUNT} ]; then
    WITH_CONFLICT=${WITH_CONFLICT} ALL_NODES_COUNT=${ALL_NODES_COUNT} PROC_COUNT=${PROC_COUNT} REPORT_DIR=`pwd` NODE_NAME=${i}@${HOST} ${RELEASE_LAUNCHER} daemon
  else
    WITH_CONFLICT=${WITH_CONFLICT} ALL_NODES_COUNT=${ALL_NODES_COUNT} PROC_COUNT=${PROC_COUNT} REPORT_DIR=`pwd` NODE_NAME=${i}@${HOST} ${RELEASE_LAUNCHER} console
  fi
done
