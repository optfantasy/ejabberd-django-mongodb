#!/bin/bash
if [ -z $1 ] || [ -z $2 ]; then
	echo "must specify the info of master servers." >&2
	echo "Usage: $0 <master_or_slave> <master_hostname> <deploy_version>" >&2
	echo "please execute this under the root of ejabberd-genie" >&2
	exit 1;
fi 

IN_TMPL=$1 # must be either "master" or "slave".
DEPLOY_VER=$2
TMPL_DIR=./config_template/${DEPLOY_VER}/node_config
IN_TMPL_PATH=${TMPL_DIR}/template_${IN_TMPL}_vars.config

if [ "$IN_TMPL" == "master" ] || [ "$IN_TMPL" == "slave" ]; then
    MASTER_HOST=$3
    OUT_TMPL_PATH=./rel/reltool_vars/${MASTER_HOST}_vars.config
elif [ "$IN_TMPL" == "global" ]; then
    OUT_TMPL_PATH=./rel/vars.config
fi

echo "Generating templates for master servers..."

# Generate the configuration file
cp ${IN_TMPL_PATH} ${OUT_TMPL_PATH}

# Modify the hostname
echo "Modifying the hostname in configuration..."
sed -i "s/{{hostname}}/${MASTER_HOST}/g" rel/reltool_vars/${MASTER_HOST}_vars.config




