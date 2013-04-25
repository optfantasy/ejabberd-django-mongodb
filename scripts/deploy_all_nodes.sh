#!/bin/bash
shopt -s expand_aliases
alias ssh="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scp="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

if [ -z $1 ]; then
    echo "Usage: $0 <deploy_version>" 1>&2
    exit 1
fi

USER=ejabberd
DEPLOY_VER=$1
DEPLOY_TABLE=./deploytables/deploy_table-${DEPLOY_VER}.csv
DEPLOY_PROXY_TABLE=./deploytables/proxy_table-${DEPLOY_VER}.csv
PROXY_HOST=`cat ${DEPLOY_PROXY_TABLE} | head -1`
PROXY_USER=ejabberd
PROXY_SETTING=~/nodes.csv
PROXY_CMD=~/deploy_haproxy.sh
TEMP_SCRIPT_NAME=ej_tmp_script.sh
FIRST_NODE=`grep "master$" $DEPLOY_TABLE | awk -F, '{ print $1 }'`

# function to execute scripts remotely.
execute_script_remote() {
    local SCRIPT_PATH=$1
    local REMOTE_USER=$2
    local REMOTE_HOST=$3
    local ARGUMENTS=${@:4}

    scp $SCRIPT_PATH $REMOTE_USER@$REMOTE_HOST:/tmp/$TEMP_SCRIPT_NAME
    ssh $REMOTE_USER@$REMOTE_HOST "chmod 755 /tmp/$TEMP_SCRIPT_NAME ; /tmp/$TEMP_SCRIPT_NAME $ARGUMENTS; chmod 644 /tmp/$TEMP_SCRIPT_NAME"
}

## Ejabberd Nodes Deployment
# clean nodes
make productionclean

# templating var.config
./scripts/gen_global_setting.sh ${DEPLOY_VER}

# Build all nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    export TARGET_LINE
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    echo "TARGET_HOST: $TARGET_HOST"
    echo "TARGET_TYPE: $TARGET_TYPE"
    # templating var_{nodes}.config for each node
    scripts/gen_${TARGET_TYPE}_setting.sh $TARGET_HOST $DEPLOY_VER
    # build each node
    make productionrel_node TARGET_PRODUCT="$TARGET_HOST"
done

# Stop all server nodes
for TARGET_HOST in `awk -F, '{print $1}' $DEPLOY_TABLE`
do
    #stop server
    execute_script_remote scripts/stop_ejabberd.sh $USER $TARGET_HOST
done

# deploy and start nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    #deploy server
    scp -q -r production/ejabberd_$TARGET_HOST $USER@$TARGET_HOST:~/ejabberd-new
    ssh $USER@$TARGET_HOST "cp -r ejabberd/Mnesia* ejabberd-new ; rm -rf ejabberd-old;  mv ejabberd ejabberd-old ; mv ejabberd-new ejabberd"

    #start server
    if [ "$TARGET_TYPE" = "slave" ]; then
        execute_script_remote scripts/start_ejabberd.sh $USER $TARGET_HOST $FIRST_NODE
    else 
        execute_script_remote scripts/start_ejabberd.sh $USER $TARGET_HOST
    fi
done

# Wait it start.
sleep 10

# After deploying, do db_sync for all slave node.
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    if [ "$TARGET_TYPE" = "slave" ]; then
        echo "Do mnesia sync @ '$TARGET_HOST'"
        execute_script_remote scripts/db_sync.sh $USER $TARGET_HOST ejabberd@$FIRST_NODE
    fi
done

# record this time deploying
git add $DEPLOY_TABLE && git commit -m "updated ${DEPLOY_TABLE}" > /dev/null


## Proxy Deployment
# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"


