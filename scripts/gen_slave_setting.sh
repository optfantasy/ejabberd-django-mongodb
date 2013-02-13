if [ -z $1 ]; then
	echo "must specify the number of slave servers." >&2
	echo "Usage: scripts/gen_slave_setting.sh <slave_num>" >&2
	exit 1;
fi 

SLAVE_NUM=$1

echo "Generating $SLAVE_NUM templates for slave servers..."

for num in $(seq 1 $SLAVE_NUM)
do
	echo make generate_setting IN_TMPL=slave OUT_TMPL=xmpp-slave-$num
	make generate_setting IN_TMPL=slave OUT_TMPL=xmpp-slave-$num
done
