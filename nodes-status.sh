#!/bin/bash
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

echo "[status] Check nodes status ---- "

# Build all nodes
for TARGET_LINE in `cat $DEPLOY_TABLE`
do
    export TARGET_LINE
    TARGET_HOST=`echo $TARGET_LINE | awk -F, '{print $1}'`
    
    ssh $TARGET_HOST "bash ~/ejabberd/bin/ejabberdctl status"
done

