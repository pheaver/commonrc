#-*-sh-*-
#####################################################
# top-level file for environment vars and settings
######################################################

test -z $shell && shell=`basename $SHELL`

export SHELL

# ----------------------------------------
# miscellaneous functions/scripts
# ----------------------------------------

function unix {
    [[ -z $ARCH ]] && export ARCH=`uname`
    if [[ $ARCH == Darwin || $ARCH == FreeBSD ]]; then
        return 0
    else
        return 1
    fi
}

function dlink {
    [[ -z $HOST ]] && export HOST=`hostname`
    if [[ $HOST == dlink* ]]; then
        return 0
    else
        return 1
    fi
}

function interactive {
    if [[ $- == *i* ]]; then
        return 0
    else
        return 1
    fi
}

function cond-source {
    test -r "$1" && source "$1" && return 0 || return 1
}

# -----------------------------------
# load other files
# -----------------------------------

#cond-source /etc/profile
#cond-source /etc/profile.env

RC=~/commonrc

cond-source /sw/bin/init.sh
cond-source ${RC}/paths.sh
cond-source ~/.localrc
if interactive; then
    cond-source ${RC}/env.sh
fi
