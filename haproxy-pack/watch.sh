#!/bin/bash
PID1=`ps ax|grep "haproxy"|grep -v "grep"|awk '{print $1}'|head -1`
INV=5
LOG=/home/ejabberd/ejabberd.stress.log
LOG_SHORT=/home/ejabberd/ejabberd.stress.short.log
SHORT_MAX=5000
COUNT=0
echo "time,connect,cpu,mem" > $LOG
while true
do
        # dynamic grab process to prevent nxb restart
        PID1=`ps ax|grep "haproxy"|grep -v "grep"|awk '{print $1}'|head -1`
        paste <(date +%s) <(ss -n |grep ":5222"|wc -l) <(top -p $PID1 -bd00.50 -n1|grep "$PID1")|awk '{print $1","$2","$11","$12}' >> $LOG
#       paste <(ss -so state established|grep ":5280"|wc -l) <(echo "--") <(top -p $PID2 bd00.50n1|grep "$PID2")
#       echo "----"

        #Make short version of log ( 5000 )
        if [ "$COUNT" -gt "$SHORT_MAX" ]; then
                paste -s -d"\n" <(echo 'time,connect,cpu,mem') <(tail -n $SHORT_MAX $LOG) > $LOG_SHORT
        else
                cp $LOG $LOG_SHORT
        fi
        COUNT=$(($COUNT+1))
        sleep $INV
done
