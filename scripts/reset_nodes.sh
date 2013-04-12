ssh xmpp-dev-master-1 "ejabberd/bin/ejabberd stop"
ssh xmpp-dev-slave-1 "ejabberd/bin/ejabberd stop"
ssh xmpp-dev-slave-2 "ejabberd/bin/ejabberd stop"
ssh xmpp-dev-slave-3 "ejabberd/bin/ejabberd stop"

ssh xmpp-dev-master-1 "rm -rf ejabberd ejabberd-old"
ssh xmpp-dev-slave-1 "rm -rf ejabberd ejabberd-old"
ssh xmpp-dev-slave-2 "rm -rf ejabberd ejabberd-old"
ssh xmpp-dev-slave-3 "rm -rf ejabberd ejabberd-old"
