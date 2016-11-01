#-*-sh-*-

# -------------------------------------
# ssh-agent

unset SSH_AUTH_SOCK
unset SSH_AGENT_PID

function agent-kill
{
#    `ssh-agent -k`
    ssh-agent -k >~/.agent &&
    . ~/.agent &&
    rm -f ~/.agent
}

function agent-start
{
    agent-kill
    ssh-agent -s > ~/.agent  &&
    . ~/.agent &&
    ssh-add `ls ~/.ssh/id* | grep -v .pub`
#    ssh-add ~/.ssh/id_rsa
}

if [ -n "$DESKTOP_SESSION" ] && type gnome-keyring-daemon >/dev/null 2>&1; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
else
    cond-source ~/.agent
fi
