#-*-sh-*-

# -------------------------------------
# environment for interactive shell
# includes aliases, exports, functions
# -------------------------------------

rc-source mail.sh
rc-source agent.sh

# -------------------------------------

# for ack
export ACK_OPTIONS="--type-set=hsc=.hsc --type-set=quattro=.q --type-set=tquattro=.q.py --type-set=cryptol=.cry --type-set=tcryptol=.cryt --type-set=verilog=.v"
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
# miscellaneous aliases and functions

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

alias pd="pushd"
# alias df="df -h"
# alias du="du -h"

alias recent="ls -FlAt | head -n 20"
alias f="find . -iname"
type psgrep >/dev/null 2>&1 || alias psgrep='ps aux | grep -v grep | grep --color=auto'
alias grep='grep --color=auto'

alias remove-temp="rm -f 0 .*~ *~ ~* \#*\# svn-*.tmp core.?????"
alias rm-temp="remove-temp"

function glocate {
    locate "$1" | grep "$2"
}

alias c="./configure --prefix=${HOME}/local"

# -----------------------------------------------
# haskell

function h {
    ghc --make Setup &&
    if [[ ! -z "$@" ]]; then ./Setup $@; fi
}

#alias hmake="runhaskell Setup.hs"
alias hconf="h configure --user"
alias hbuild="h build"
alias hinstall="h build && h install"
alias hall="hconf && hbuild && hinstall"

alias gentags="/usr/bin/find . -iname \*.\*hs | xargs hasktags -a -e "

function use-ghc {
    version=$1
    if [ `which ghc-${version}` >/dev/null 2>&1 ]; then
        export GHC=ghc-${version}
        export GHC_PKG=ghc-pkg-${version}
        alias ghc=${GHC}
        alias ghc-pkg=${GHC_PKG}
        alias ghci=ghci-${version}
    else
        echo "Could not find ghc version ${version}"
    fi
}

# -----------------------------------------------
# tmux, local network, and ssh

alias screen="screen -D -R"
alias sn="screen"

nas=192.168.1.60
bob=192.168.1.35
cheese=192.168.1.40

for x in nas bob cheese; do
    # TODO use create-or-attach logic
    alias $x="ssh $x -t tmux a"
done

# -----------------------------------------------
# transmission and torrents

function submit_torrents
{
    # can't figure out an easier way to check if any .torrent files exist,
    # so this is what we have for now...
    test -n "`ls *.torrent 2>/dev/null`" &&
    rsync -v --remove-source-files *.torrent cheese:/mnt/storage/watch
}

if dlink; then
   alias tra="transmission-remote $nas"
else
   alias tra="transmission-remote $cheese"
fi

alias reload-transmission="killall -HUP transmission-daemon"

function tl {
  if [ -z $1 ]; then
    tra -l
  else
    tra -l | grep -i $1
  fi
}

################################################################################
