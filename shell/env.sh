#-*-sh-*-

# environment for interactive shell
# includes aliases, exports, functions

rc-source mail.sh
rc-source agent.sh

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

function colours {
    for i in {0..255} ; do
        printf "\x1b[38;5;${i}mcolour${i}\n"
    done
}

# haskell
alias gentags="/usr/bin/find . -iname \*.\*hs | xargs hasktags -a -e "
alias i="stack ghci --ghci-options -j4"
alias b="stack build --fast -j2 --ghc-options -j4"

# ssh stuff
alias cheese="ssh 192.168.1.40 -t tmux a"

# trun
function trun {
  if [ $# -lt 4 ]; then
      echo "trun: not enough arguments" >&2
      return 1
  fi
  SESSION="$1"
  WINDOW="$2"
  DIR="$3"
  CMD="$4"
  tmux has-session -t "$SESSION" 2>/dev/null || tmux new-session -d -s "$SESSION"
  tmux new-window -d -P -t "$SESSION" -n "$WINDOW" -c "$DIR" "$CMD" \; set-window-option -t "$SESSION:$WINDOW" remain-on-exit on
}

# transmission and torrents
function submit_torrents
{
    # can't figure out an easier way to check if any .torrent files exist,
    # so this is what we have for now...
    test -n "`ls *.torrent 2>/dev/null`" &&
    rsync -v --remove-source-files *.torrent cheese:/mnt/storage/watch
}

alias tra="transmission-remote $cheese"

alias reload-transmission="killall -HUP transmission-daemon"

function tl {
  if [ -z $1 ]; then
    tra -l
  else
    tra -l | grep -i $1
  fi
}
