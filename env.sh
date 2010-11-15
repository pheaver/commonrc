#-*-sh-*-

# -------------------------------------
# environment for interactive shell
# includes aliases, exports, functions
# -------------------------------------

cond-source ${RC}/mail.sh
cond-source ${RC}/agent.sh

# -------------------------------------

# for ack
export ACK_OPTIONS="--type-set=quattro=.q --type-set=tquattro=.q.py --type-set=cryptol=.cry --type-set=tcryptol=.cryt --type-set=verilog=.v"
export ACK_PAGER_COLOR="less -R -F"
export CLICOLOR=yes

export LESS=FRX

# for LaTeX
export TEXINPUTS=.:${RC}/tex:$TEXINPUTS
export BSTINPUTS=.:${RC}/tex:$BSTINPUTS
export BIBINPUTS=.:${RC}/tex:$BIBINPUTS
export TEXMFOUTPUT=/tmp

export RSYNC_RSH=ssh

export QUATTROFLAGS="outdir=${HOME}/outdir outfile=foo lineLength=105"

# -------------------------------------

if unix; then
    alias ls="ls -G"
elif [[ -z $EMACS ]]; then
    alias ls="ls --color=auto"
else
    alias ls="ls"
fi

alias cd..="cd .."
alias ..="cd .."
alias l="ls"
alias d="ls"
alias ll="ls -lh"
alias lt="ls -lhtr"
alias lta="ls -lhtra"
alias lat="ls -lhtra"
alias la="ls -A"
alias lla="ls -A -hl"
alias lal="ls -A -hl"

# simple miscellaneous stuff
alias pd="pushd"
alias df="df -h"
alias du="du -h"

alias recent="ls -FlAt | head -n 20"
alias find="find . -iname"
alias psgrep='ps aux | grep -v grep | grep --color=auto'
alias grep='grep --color=auto'

alias gentags="/usr/bin/find . -iname \*.\*hs | xargs hasktags -a -e "

function h {
#    if [ "`which cabal`" ]; then
#       cabal $@
#    else
        ghc --make Setup &&
        if [[ ! -z "$@" ]]; then ./Setup $@; fi
#    fi
}

#alias hmake="runhaskell Setup.hs"
alias hconf="h configure --user"
alias hbuild="h build"
alias hinstall="h build && h install"
alias hall="hconf && hbuild && hinstall"

alias c="./configure --prefix=${HOME}/local"

# remove, reload, etc...
alias remove-temp="rm -f 0 .*~ *~ ~* \#*\# svn-*.tmp core.?????"
alias rm-temp="remove-temp"

alias screen="screen -D -R"
alias sn="screen"

function glocate {
    locate "$1" | grep "$2"
}

function submit_torrents
{
    test -e *.torrent && rsync -v --remove-source-files *.torrent pjw:~/downloads/torrents/watch
}

# -----------------------------------------------
# ssh and local network

max=192.168.1.40
deb=192.168.1.50
nas=192.168.1.60

# ssh aliases
#alias barry="ssh barry -Y"
alias tux="ssh tux -Y"

alias barry="ssh barry -t screen -D -R"
alias nas="ssh nas -t screen -D -R"
alias deb="ssh deb -t screen -D -R"
alias pjw="ssh pjw -t screen -D -R"
alias ubu="ssh ubu -t screen -D -R"

alias sunfire="ssh sunfire -t screen -D -R"
alias signali="ssh signali -t screen -D -R"

# -----------------------------------------------
# transmission

alias tra="transmission-remote $nas"

alias reload-transmission="killall -HUP transmission-daemon"

function tl {
  if [ -z $1 ]; then
    tra -l
  else
    tra -l | grep -i $1
  fi
}

################################################################################
