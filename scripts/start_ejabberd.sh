EJABBERD_DIR=ejabberd
EXISTS_MNESIA_DIR=`ls -d1 ejabberd-old 2> /dev/null | wc -l`
FIRSTNODE=$1

ejabberd/bin/ejabberd start

if [ "$EXISTS_MNESIA_DIR" = "0" ] && [ -n "$FIRSTNODE" ]; then
    echo "This machine seems like being deploying first time, do dbsync"
    sleep 3 && ejabberd/bin/ejabberdctl mnesia_slave_dbsync $FIRSTNODE
    ejabberd/bin/ejabberd stop && ejabberd/bin/ejabberd start
fi

