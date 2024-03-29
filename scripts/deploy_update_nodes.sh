#!/bin/bash
shopt -s expand_aliases
alias ssh="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scp="scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

if [ -z $1 ]; then
    echo "Usage: $0 <deploy_version>"
    exit 1
fi

USER=ejabberd
DEPLOY_VER=$1
TEMPLATE_FOLDER=./config_template/${DEPLOY_VER}
TEMPLATE_TABLE_FOLDER=${TEMPLATE_FOLDER}/deploy_table
DEPLOY_TABLE=${TEMPLATE_TABLE_FOLDER}/deploy_table.csv
DEPLOY_PROXY_TABLE=${TEMPLATE_TABLE_FOLDER}/proxy_table.csv
PROXY_USER=ejabberd
PROXY_HOST=`cat ${DEPLOY_PROXY_TABLE} | head -1`
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

# function to deploy a new node
# usage: add_node <node_host> <node_type>
build_node () {
    TARGET_HOST=$1
    TARGET_TYPE=$2

    scripts/gen_setting.sh ${TARGET_TYPE} ${DEPLOY_VER} ${TARGET_HOST}

    make productionrel_node TARGET_PRODUCT="$TARGET_HOST"
}
add_node () {
    TARGET_HOST=$1
    TARGET_TYPE=$2

    scp -q -r production/ejabberd_$TARGET_HOST $USER@$TARGET_HOST:~/ejabberd-new
    ssh $USER@$TARGET_HOST "cp -r ejabberd/Mnesia* ejabberd-new ; rm -rf ejabberd-old;  mv ejabberd ejabberd-old ; mv ejabberd-new ejabberd"
    execute_script_remote scripts/remote_commands/start_ejabberd.sh $USER $TARGET_HOST
}

# function to stop a node
# usage: remove_node <node_host>
remove_node() {
	TARGET_HOST=$1
	execute_script_remote scripts/remote_commands/stop_ejabberd.sh $USER $TARGET_HOST
}


make productionclean

# templating var.config
./scripts/gen_global_setting.sh ${DEPLOY_VER}


diff_result=`scripts/diff_deploy_table.sh ${DEPLOY_VER}`
#diff_result=`cat testdiff` # for testing

nodes_add=`echo "$diff_result" | grep "^+" --color=never | awk '{ print $2; }'`
nodes_remove=`echo "$diff_result" | grep "^-" --color=never | awk '{ print $2; }'`

echo $nodes_add
echo $nodes_remove


# building new nodes...
for TARGET_LINE in $nodes_add  
do
    NODE_HOST=`echo $TARGET_LINE | awk -F"," '{ print $1 }'`
    NODE_TYPE=`echo $TARGET_LINE | awk -F"," '{ print $2 }'`

    echo "build_node $NODE_HOST $NODE_TYPE"
    build_node $NODE_HOST $NODE_TYPE
done

# starting new nodes...
for TARGET_LINE in $nodes_add  
do
    NODE_HOST=`echo $TARGET_LINE | awk -F"," '{ print $1 }'`
    NODE_TYPE=`echo $TARGET_LINE | awk -F"," '{ print $2 }'`
    echo "add_node $NODE_HOST $NODE_TYPE"
    add_node $NODE_HOST $NODE_TYPE
done

# stopping deprecated nodes...
for TARGET_LINE in $nodes_remove
do
    NODE_HOST=`echo $TARGET_LINE | awk -F"," '{ print $1 }'`
    echo "remove_node $NODE_HOST $NODE_TYPE"
    remove_node $NODE_HOST $NODE_TYPE
done

# wait ej start

sleep 10

# do db_sync
for TARGET_LINE in $nodes_add  
do
    NODE_HOST=`echo $TARGET_LINE | awk -F"," '{ print $1 }'`
    NODE_TYPE=`echo $TARGET_LINE | awk -F"," '{ print $2 }'`
    echo "Do mnesia sync @ '$NODE_HOST'"

    if [ "$NODE_TYPE" = "slave" ]; then
    execute_script_remote scripts/remote_commands/db_sync.sh $USER $NODE_HOST ejabberd@$FIRST_NODE
    fi
done

# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"
