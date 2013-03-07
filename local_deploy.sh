#!/bin/bash

USER=ejabberd
DEPLOY_TABLE=deploy_table.csv
PROXY_USER=ejabberd
PROXY_HOST=xmpp-dev-proxy-1
PROXY_SETTING=~/nodes.csv
PROXY_CMD=~/deploy_haproxy.sh

make productionclean

# Stop all server nodes
for TARGET_HOST in `awk -F, '{print $1}'`
do
    #stop server
    ssh $USER:$TARGET_HOST 'bash -s' < scripts/stop_ejabberd.sh
done

# deploy and start nodes
for TARGET_HOST in `awk -F, '{print $1}'`
do
    #deploy
    make productionrel_node TARGET_PRODUCT="$TARGET_HOST"
    scp -r production/ejabberd_$TARGET_HOST $USER@$TARGET_HOST:~/ejabberd
    #start server
    ssh $USER:$TARGET_HOST 'bash -s' < scripts/start_ejabberd.sh
done

# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"
