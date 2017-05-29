#!/bin/sh

# set keyboard repeat delay and rate
xset r rate 300 40

# Allows Ctrl+Alt+Backspace to restart Xorg
setxkbmap -option terminate:ctrl_alt_bksp

# should remap Caps_Lock as Control_L
# xmodmap ~/.Xmodmap

# make short-pressed Ctrl behave like Escape:
# xcape -e 'Control_L=Escape'

if type croutonversion >/dev/null; then
    setxkbmap -option ctrl:swap_lwin_lctl

    # Right Alt as Super
    xmodmap -e "remove mod1 = Alt_R"
    xmodmap -e "clear mod4"
    xmodmap -e "keycode 108 = Super_R"
    xmodmap -e "add mod4 = Super_R"
else
    # make CapsLock behave like Ctrl:
    setxkbmap -option ctrl:nocaps
fi

