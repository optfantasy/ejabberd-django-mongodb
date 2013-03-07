EJABBERD_DIR=ejabberd
EXISTS_MNESIA_DIR=`ls -d1 ejabberd_xmpp-dev-master-1/Mnesia* 2> /dev/null | wc -l`
FIRSTNODE=xmpp-dev-master-1


#if [ "$EXISTS_MNESIA_DIR" = 0 ]; then
#    ejabberd/bin/ejabberdctl mnesia_slave_dbsync $FIRSTNODE
#fi

ejabberd/bin/ejabberd start
