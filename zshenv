#-*-sh-*-

shell=zsh

#setopt NULL_GLOB               # if pattern has no matches, delete pattern instead of reporting error
unsetopt NOMATCH                # if pattern has no matches, leave it alone
#unsetopt GLOB
unsetopt EQUALS

test -r ~/commonrc/shell/init.sh && source ~/commonrc/shell/init.sh
