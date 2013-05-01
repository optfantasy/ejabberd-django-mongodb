EJABBERD_DIR=/home/ejabberd/ejabberd
FIRSTNODE=$1

$EJABBERD_DIR/bin/ejabberdctl mnesia_slave_dbsync $FIRSTNODE
$EJABBERD_DIR/bin/ejabberd stop && $EJABBERD_DIR/bin/ejabberd start

