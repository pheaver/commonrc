#-*-sh-*
################################################################################
# required variables and functions that should be loaded before anything else
################################################################################

test -z $shell && shell=`basename $SHELL`

export SHELL

# ----------------------------------------
# miscellaneous functions/scripts
# ----------------------------------------

function unix {
    [[ -z $MYARCH ]] && export MYARCH=`uname`
    if [[ $MYARCH == Darwin || $MYARCH == FreeBSD ]]; then
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

################################################################################
