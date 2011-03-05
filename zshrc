#-*-sh-*-

# ~/.zshrc
# loaded for interactive shell, both login and non-login
# TODO: most of the stuff in this file should only be loaded for login shells

# zsh options:
HISTFILE=$HOME/.zhistory
HISTSIZE=1000
SAVEHIST=1000

#setopt SHARE_HISTORY
#setopt HIST_IGNORE_ALL_DUPS
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt NO_HUP
#setopt NO_CHECK_JOBS

alias re-source="source ~/.zshenv && source ~/.zshrc"
alias rm="nocorrect rm"
alias find="noglob find . -iname"

# if not interactive...
#if [[ $- != *i* ]]; then
#    return
#fi

####################################################
# how the login prompt appears

# http://aperiodic.net/phil/prompt/
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

typeset -A altchar
set -A altchar ${(s..)terminfo[acsc]}
PR_SET_CHARSET="%{$terminfo[enacs]%}"
PR_SHIFT_IN="%{$terminfo[smacs]%}"
PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
PR_HBAR=${altchar[q]:--}
PR_ULCORNER=${altchar[l]:--}
PR_LLCORNER=${altchar[m]:--}
PR_LRCORNER=${altchar[j]:--}
PR_URCORNER=${altchar[k]:--}

# %n is username
# %m is hostname
# %d is directory
# %1~ is directory basename, home replaced with ~

PR_USER=%(!.${PR_LIGHT_RED}.${PR_LIGHT_GREEN})%n
PR_DIR=${PR_LIGHT_BLUE}%~${PR_NO_COLOR}

jobs=${PR_BLUE}%(1j. %j.)${PR_NO_COLOR}
exitstatus=${PR_RED}%(0?.. %?)${PR_NO_COLOR}
PS1="${PR_NO_COLOR}[%m %D{%I:%M:%S}${jobs}${exitstatus}] ${PR_USER} ${PR_DIR}
%(!.#.$) "   # '#' if running with priveleges, '$' otherwise

#PS1="%# '${vcs_info_msg_0_}'"

#RPROMPT="[${jobs}${exitstatus}%*]"
#defaults:
#PS2="%_> "     # printed when the shell needs more information to complete a command
#PS3="?# "      # when in a select loop
#PS4="+%N:%i> " # execution trace prompt

########################################################
# dynamic terminal title

if test "$EMACS" == "t"; then
    unsetopt zle
    export TERM=xterm-color
else
    precmd () {
        case $TERM in
            xterm*|rxvt|vt*)
                print -Pn "\e]0;%m : %3~\a"
                ;;
            screen)
                print -Pn "\e]0;%m : %3~\a\ek%m : %3~\e\\"
                ;;
        esac
    }

    preexec () {
        case $TERM in
            xterm*|rxvt|vt*)
                print -Pn "\e]0;%m : $1\a"
                ;;
            screen)
                print -Pn "\e]0;%m : $1\a\ek%m : $1\e\\"
                ;;
        esac
    }
fi

# --------------------------------------------
# fix a problem with tramp

if [ "$TERM" = "dumb" ]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi

# bindkeys
# --------------------------------------------

# TODO: make all of these bindkeys dependent of $TERM

# bindkey "^[[H" beginning-of-line  # Home
# bindkey "^[[F" end-of-line  # Home

# bindkey '\e[5~' history-search-backward
# bindkey '\e[6~' history-search-forward

# case $TERM in (xterm*|rxvt)
#     bindkey '\e[7~' beginning-of-line
#     bindkey '\e[8~' end-of-line ;;
#     (*)
#     bindkey '\e[1~' beginning-of-line
#     bindkey '\e[4~' end-of-line ;;
# esac

# bindkey '^[[3~' backward-delete-char

#delete-char
#bindkey '^?' backward-delete-char
#bindkey '^H' backward-delete-char

# emacs bindings
bindkey -e

# -----------------------------------------------

# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

#zstyle ':completion:*' verbose yes
#zstyle ':completion:*:descriptions' format '%B%d%b'
#zstyle ':completion:*:messages' format '%d'
#zstyle ':completion:*:warnings' format 'No matches for: %d'
#zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
#zstyle ':completion:*' group-name ''

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _approximate
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/pweaver/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
