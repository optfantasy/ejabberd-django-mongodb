git add ./deploy_table.csv && git commit -m 'Update deploy_table.csv' > /dev/null
git diff HEAD^ HEAD ./deploy_table.csv |grep ^[+-][^+-] --color=never|sed 's/^\([+-]\)/\1 /'

