#-*-sh-*-
# 
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.

#echo .bashrc

#[[ -f /etc/profile.env ]] && . /etc/profile.env

shell=bash

[[ -r ~/commonrc/master.sh ]] && . ~/commonrc/master.sh

if ! interactive; then
        return
fi

if [ "$UID" -eq 0 ]; then
    PS1='[\h] \[\033[01;31m\]\u \[\033[01;34m\]\w\[\033[00m\]
# '
else
    PS1='[\h] \[\033[01;32m\]\u \[\033[01;34m\]\w\[\033[00m\]
$ '
#    PS1='[\h] \[\e[32m\]\u \[\e[33m\]\w\[\033[00m\]\n\$ '
fi

export PS1

# the prompt at the top of term windows
unset PROMPT_COMMAND
case $TERM in
    *term | rxvt )
	PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}: ${PWD}\007"' ;;
    *)
esac

#xtitle () 
#{ 
#    echo -n -e "\033]0;$*\007"
#}

#PROMPT_COMMAND='xtitle $(jobs %+)'

alias re-source="source ~/.bashrc"

# uncomment the following to activate bash-completion:
[ -f /etc/profile.d/bash-completion ] && source /etc/profile.d/bash-completion
