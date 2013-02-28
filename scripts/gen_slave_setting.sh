#!/bin/bash
if [ -z $1 ]; then
	echo "must specify the info of slave servers." >&2
	echo "Usage: scripts/gen_slave_setting.sh <slave_hostname>" >&2
	exit 1;
fi 

SLAVE_HOST=$1

echo "Generating templates for slave servers..."

# Generate the configuration file
echo make generate_setting IN_TMPL=slave OUT_TMPL=${SLAVE_HOST}
make generate_setting IN_TMPL=slave OUT_TMPL=${SLAVE_HOST}

# Modify the hostname
echo "Modifying the hostname in configuration..."
sed -i "s/{{hostname}}/${SLAVE_HOST}/g" rel/reltool_vars/${SLAVE_HOST}_vars.config

