#-*-sh-*-

################################################################################
# setup functions, aliases, and variables related to emacs
################################################################################

emacs=`which emacs`
emacsclient=`which emacsclient`
emacs_major_version=`emacsclient --version 2>&1 | awk '{ print substr($2,1,2) }'`
# emacs_major_version=`emacs --no-site-file --no-init-file --batch \
#                      --eval "(princ (format \"%i\\n\" emacs-major-version))"`

alias eq="${emacs} -nw -Q"
alias ea="${emacsclient} -n"
alias ec="${emacsclient} -n -c"

function er {
    if [ ! -z "$1" ]; then
        ${emacsclient} -n "$1"
    fi
    ${emacsclient} -e '(phil/ns-raise-emacs)'
}

# open a function in emacs daemon with root priveleges using sudo
function sem {
    ${emacsclient} -e "(phil/find-file-sudo \"$1\")"
}

# open in emacs all files matching pattern
function fm {
    /usr/bin/find . -iname "${1}" | xargs ${emacsclient} -n
}

if [ $UID -eq 0 ]; then
    alias em="emacs -nw"
    export EDITOR=emacs
elif [[ ! -z "$EMACS" ]]; then
    # we are running a terminal inside emacs
    export EDITOR="${emacsclient}"
elif [[ ${emacs_major_version} == "23" ]]; then
    alias em="\"${emacsclient}\" -t"
    export EDITOR="${emacsclient} -t"
    export VISUAL="${emacsclient}"

    # emacsclient: try to run emacs --daemon if server is not running.
    export ALTERNATE_EDITOR=""
else
    alias em="emacs -nw"
    export EDITOR="emacs"
fi

#export GIT_EDITOR="${EDITOR}"

function emake {
    ${emacsclient} -e "(compile \"make $@\" t)"
}

alias kill-emacs="\"${emacsclient}\" -a \"false\" -e '(kill-emacs)'"

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

################################################################################
