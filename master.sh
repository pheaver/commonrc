#-*-sh-*-
#####################################################
# master rc file that should be called by
# top-level shell file (~/.zshenv or ~/.bashrc)
######################################################

# TODO infer RC by the location of this file (master.sh)
test -z ${RC} && RC=~/commonrc
test -f ${RC}/init.sh && source ${RC}/init.sh

if [[ -f ~/.master-rc ]]; then
    source ~/.master-rc
else
    cond-source ${RC}/paths.sh
    cond-source ~/.localrc # for backwards compatibility
    if interactive; then
        cond-source ${RC}/env.sh
        cond-source ${RC}/emacs.sh
    fi
fi
