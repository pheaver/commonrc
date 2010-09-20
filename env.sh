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
    for i in *.torrent; do
        scp -- "$i" pjw:~/downloads/torrents/watch &&
        rm -- "$i"
    done
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

# -----------------------------------------------
# emacs stuff

emacs=`which emacs`
emacsclient=`which emacsclient`
emacs_major_version=`emacsclient --version 2>&1 | awk '{ print substr($2,0,2) }'`
# emacs_major_version=`emacs --no-site-file --no-init-file --batch \
#                      --eval "(princ (format \"%i\\n\" emacs-major-version))"`

alias eq="${emacs} -nw -Q"
alias ea="${emacsclient} -n"
alias ec="${emacsclient} -n -c"

function er {
    if [ ! -z "$1" ]; then
        ${emacsclient} -n "$1"
    fi
    ${emacsclient} -e '(ns-raise-emacs)'
}

# open a function in emacs daemon with root priveleges using sudo
function sem {
    ${emacsclient} -e "(find-file-sudo \"$1\")"
}

if [ $UID -eq 0 ]; then
    alias em="emacs -nw"
    export EDITOR=emacs
elif [[ ! -z "$EMACS" ]]; then
    # we are running a terminal inside emacs
    export EDITOR="${emacsclient}"
elif [[ ${emacs_major_version} == "23" ]]; then
    alias em="${emacsclient} -t"
    export EDITOR="${emacsclient} -t"
    export VISUAL="${emacsclient}"

    # emacsclient: try to run emacs --daemon if server is not running.
    export ALTERNATE_EDITOR=""
else
    alias em="emacs -nw"
    export EDITOR="emacs"
fi

export GIT_EDITOR="${EDITOR}"

function emake {
    ${emacsclient} -e "(compile \"make $@\" t)"
}

alias kill-emacs="${emacsclient} -e '(kill-emacs)'"

#export VISUAL="${emacsclient}"

# if we're inside emacs shell-mode, but not term-mode
if [[ "$EMACS" == "t" ]]; then
  function man {
      if [[ "$2" == "" ]]; then
          ea -e "(man \"$1\")"
      else
          ea -e "(man \"$2($1)\")"
      fi
  }

  export PAGER="cat"
fi
