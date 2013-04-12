
if [ -z "$1" ]; then
    echo "Usage: $0 <deploy_version>" 1>&2
fi

DEPLOY_VER=$1


TMPL_IN="./rel/var.config-versions/vars-${DEPLOY_VER}.config"
TMPL_TARGET="./rel/vars.config"

cp ${TMPL_IN} ${TMPL_TARGET}

