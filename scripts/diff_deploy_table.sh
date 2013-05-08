
DEPLOY_VER=$1
DEPLOY_TABLE_FILE=./deploytables/deploy_table-${DEPLOY_VER}.csv

git add $DEPLOY_TABLE_FILE && git commit -m "Update deploy_table-${DEPLOY_VER}.csv" > /dev/null
git diff HEAD^ HEAD $DEPLOY_TABLE_FILE |grep ^[+-][^+-] --color=never|sed 's/^\([+-]\)/\1 /'

