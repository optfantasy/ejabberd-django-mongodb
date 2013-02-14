#!/bin/bash
if [ -z $1 ]; then
	echo "must specify the info of master servers." >&2
	echo "Usage: scripts/gen_master_setting.sh <master_hostname>" >&2
	exit 1;
fi 

MASTER_HOST=$1

echo "Generating templates for master servers..."

# Generate the configuration file
echo make generate_setting IN_TMPL=master OUT_TMPL=${MASTER_HOST}
make generate_setting IN_TMPL=master OUT_TMPL=${MASTER_HOST}

# Modify the hostname
echo "Modifying the hostname in configuration..."
sed -i "s/{{hostname}}/${MASTER_HOST}/g" rel/reltool_vars/${MASTER_HOST}_vars.config

