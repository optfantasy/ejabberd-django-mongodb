#!/bin/bash

USER=ejabberd
DEPLOY_TABLE=deploy_table.csv
PROXY_USER=ejabberd
PROXY_HOST=xmpp-dev-proxy-1
PROXY_SETTING=~/nodes.csv
PROXY_CMD=~/deploy_haproxy.sh
TEMP_SCRIPT_NAME=ej_tmp_script.sh

# function to execute scripts remotely.
execute_script_remote() {
    local SCRIPT_PATH=$1
    local REMOTE_USER=$2
    local REMOTE_HOST=$3
    local ARGUMENTS=${@:4}

    scp $SCRIPT_PATH $REMOTE_USER@$REMOTE_HOST:/tmp/$TEMP_SCRIPT_NAME
    ssh $REMOTE_USER@$REMOTE_HOST "chmod 755 /tmp/$TEMP_SCRIPT_NAME ; /tmp/$TEMP_SCRIPT_NAME $ARGUMENTS; chmod 644 /tmp/$TEMP_SCRIPT_NAME"
}

make productionclean

# Stop all server nodes
for TARGET_HOST in `awk -F, '{print $1}' $DEPLOY_TABLE`
do
    #stop server
    #ssh $USER:$TARGET_HOST 'bash -s' < scripts/stop_ejabberd.sh
    execute_script_remote scripts/stop_ejabberd.sh $USER $TARGET_HOST
done

# deploy and start nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    export TARGET_LINE
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    TARGET_TYPE=`echo $TARGET_LINE | awk -F, '{print $2}'`

    echo "TARGET_HOST: $TARGET_HOST"
    echo "TARGET_TYPE: $TARGET_TYPE"
    #deploy
    #make generate_setting IN_TMPL=$TARGET_TYPE OUT_TMPL=$TARGET_HOST
    scripts/gen_${TARGET_TYPE}_setting.sh $TARGET_HOST

    make productionrel_node TARGET_PRODUCT="$TARGET_HOST"
    scp -r production/ejabberd_$TARGET_HOST $USER@$TARGET_HOST:~/ejabberd-new
    ssh $USER@$TARGET_HOST "mv ejabberd ejabberd-old && cp -r ejabberd-old/Mnesia* ejabberd-new ; mv ejabberd-new ejabberd"
    #start server
    #ssh $USER:$TARGET_HOST 'bash -s' < scripts/start_ejabberd.sh
    execute_script_remote scripts/start_ejabberd.sh $USER $TARGET_HOST
done

# Proxy setting
scp $DEPLOY_TABLE $PROXY_USER@$PROXY_HOST:$PROXY_SETTING

# Run HAProxy
ssh $PROXY_USER@$PROXY_HOST "bash $PROXY_CMD"
