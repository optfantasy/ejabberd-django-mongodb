#!/bin/sh

TPLDIR=~/haproxy-templating
HACMD=/usr/local/bin/haproxy_wrapper.sh
CONFIG=~/ejabberd.cfg
NODELIST=~/nodes.csv
PYTHON=python

sudo killall haproxy
$PYTHON $TPLDIR/main.py $HACMD $CONFIG $NODELIST
