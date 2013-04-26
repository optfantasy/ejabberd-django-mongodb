EJABBERD_DIR="ejabberd"

if [ -d "$EJABBERD_DIR" ]; then
    ejabberd/bin/ejabberd stop
fi

