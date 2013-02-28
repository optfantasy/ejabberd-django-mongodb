#!/bin/bash

USER=ejabberd
DEPLOY_TABLE=deploy_table.csv
PROXY_USER=ejabberd
PROXY_HOST=xmpp-dev-proxy-1
PROXY_SETTING=~/nodes.csv
PROXY_CMD=~/deploy_haproxy.sh

make productionclean
#make productionrel
#scp -r production/ejabberd_production1 ubuntu@empty:~/
awk -F, '{print $1}' $DEPLOY_TABLE | xargs -i make productionrel_node TARGET_PRODUCT="{}"
awk -F, '{print $1}' $DEPLOY_TABLE | xargs -i scp -r production/ejabberd_{} $USER@{}:~/

# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"
