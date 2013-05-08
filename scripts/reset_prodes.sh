ssh xmpp-prod-master-1 "ejabberd/bin/ejabberd stop"
ssh xmpp-prod-slave-1 "ejabberd/bin/ejabberd stop"
ssh xmpp-prod-slave-2 "ejabberd/bin/ejabberd stop"
ssh xmpp-prod-slave-3 "ejabberd/bin/ejabberd stop"

ssh xmpp-prod-master-1 "rm -rf ejabberd ejabberd-old"
ssh xmpp-prod-slave-1 "rm -rf ejabberd ejabberd-old"
ssh xmpp-prod-slave-2 "rm -rf ejabberd ejabberd-old"
ssh xmpp-prod-slave-3 "rm -rf ejabberd ejabberd-old"
