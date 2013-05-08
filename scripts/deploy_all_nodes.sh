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
TEMPLATE_FOLDER=./config_template/${DEPLOY_VER}
TEMPLATE_TABLE_FOLDER=${TEMPLATE_FOLDER}/deploy_table
DEPLOY_TABLE=${TEMPLATE_TABLE_FOLDER}/deploy_table.csv
DEPLOY_PROXY_TABLE=${TEMPLATE_TABLE_FOLDER}/proxy_table.csv
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
./scripts/gen_setting.sh global ${DEPLOY_VER}

# Build all nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    export TARGET_LINE
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    echo "TARGET_HOST: $TARGET_HOST"
    echo "TARGET_TYPE: $TARGET_TYPE"
    # templating var_{nodes}.config for each node
    scripts/gen_setting.sh $TARGET_TYPE $DEPLOY_VER $TARGET_HOST
    # build each node
    make productionrel_node TARGET_PRODUCT="$TARGET_HOST"
done

# Stop all server nodes
for TARGET_HOST in `awk -F, '{print $1}' $DEPLOY_TABLE`
do
    #stop server
    execute_script_remote scripts/remote_commands/stop_ejabberd.sh $USER $TARGET_HOST
done

echo "[deploy_nodes] Stage deploy_nodes ...... [INIT]"

# deploy and start nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    #deploy server
    scp -q -r production/ejabberd_$TARGET_HOST $USER@$TARGET_HOST:~/ejabberd-new
    #backup old snapshot
    ssh $USER@$TARGET_HOST "cp -r ejabberd/Mnesia* ejabberd-new ; rm -rf ejabberd-old;  mv ejabberd ejabberd-old ; mv ejabberd-new ejabberd"

    #start server
    execute_script_remote scripts/remote_commands/start_ejabberd.sh $USER $TARGET_HOST
done

echo "[deploy_nodes] Stage db_sync ...... [OK]"

# Wait it start.

echo "[dbsync] Stage db_sync ...... [INIT]"

INV=2
IS_RUNNING=`ssh $FIRST_NODE "bash ~/ejabberd/bin/ejabberdctl mnesia"|grep is_running|awk -F, '{print $2}'|sed 's/}$//'`

while [ "yes" != "$IS_RUNNING" ]
do
    sleep $INV
    echo "sleep & test mnesia @ $FIRST_NODE"
    IS_RUNNING=`ssh $FIRST_NODE "bash ~/ejabberd/bin/ejabberdctl mnesia"|grep is_running|awk -F, '{print $2}'|sed 's/}$//'`
done

# After deploying, do db_sync for all slave node.
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    if [ "$TARGET_TYPE" = "slave" ]; then
        echo "Do mnesia sync @ '$TARGET_HOST'"
        execute_script_remote scripts/remote_commands/db_sync.sh $USER $TARGET_HOST ejabberd@$FIRST_NODE
    fi
done

echo "[dbsync] Stage db_sync ...... [OK]"

# record this time deploying
git add $DEPLOY_TABLE && git commit -m "updated ${DEPLOY_TABLE}" > /dev/null


## Proxy Deployment
# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"


