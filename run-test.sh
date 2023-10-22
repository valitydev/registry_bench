#!/bin/bash

ALL_NODES_COUNT=$1
PROC_COUNT=$2
WITH_CONFLICT=$3
HOST=`hostname -f`
RELEASE_LAUNCHER="_build/default/rel/registry_bench/bin/registry_bench"

echo "Start consul nodes"
sudo docker run --rm -d -p 8500:8500 --name=consul-0 consul:1.15 agent -server -ui -node=consul-0 -client=0.0.0.0 -bootstrap-expect=$ALL_NODES_COUNT
CONSUL_ADDR=`sudo docker exec consul-0 consul members | awk ' /172/{print $2}' | cut -d':' -f1`
for ((i=1; i < ALL_NODES_COUNT ; i++)); do
  let "CONSUL_PORT=8500+$i"
  CONSUL_NODE="consul-$i"
  sudo docker run --rm -d -p $CONSUL_PORT:8500 --name=$CONSUL_NODE consul:1.15 agent -server -ui -node=$CONSUL_NODE -client=0.0.0.0 -retry-join=$CONSUL_ADDR
done

echo "Wait consul consensus"
sleep 3

rebar3 release
echo
for ((i=1; i <= ALL_NODES_COUNT ; i++)); do
  echo "$i node starting"
  if [ $i -ne ${ALL_NODES_COUNT} ]; then
    let "CONSUL_PORT=8500+$i"
    WITH_CONFLICT=${WITH_CONFLICT} ALL_NODES_COUNT=${ALL_NODES_COUNT} PROC_COUNT=${PROC_COUNT} REPORT_DIR=`pwd` NODE_NAME=${i}@${HOST} CONSUL_NODE="consul-$i" CONSUL_URL="http://$HOST:$CONSUL_PORT" ${RELEASE_LAUNCHER} daemon
  else
    WITH_CONFLICT=${WITH_CONFLICT} ALL_NODES_COUNT=${ALL_NODES_COUNT} PROC_COUNT=${PROC_COUNT} REPORT_DIR=`pwd` NODE_NAME=${i}@${HOST} CONSUL_NODE="consul-0" CONSUL_URL="http://$HOST:8500" ${RELEASE_LAUNCHER} console
  fi
done
rebar3 clean

echo "Stop consul nodes"
for ((i=0; i < ALL_NODES_COUNT ; i++)); do
  CONSUL_NODE="consul-$i"
  sudo docker stop $CONSUL_NODE
done
