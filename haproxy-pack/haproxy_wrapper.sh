#!/bin/sh

CONFIG=$1
if [ ! -f "$CONFIG" ]; then
	echo "[ERROR] Can't find haproxy config file : $CONFIG ."
	exit 1
fi

sudo haproxy -D -f $CONFIG 
