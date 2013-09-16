#-*-sh-*-
#####################################################
# default master file that is called if ~/.localrc/master.sh does not exist
######################################################

rc-source paths.sh
cond-source ~/.localrc # for backwards compatibility
if interactive; then
    rc-source env.sh
    if dlink; then
        export EDITOR=vi
    else
        rc-source emacs.sh
    fi
fi
