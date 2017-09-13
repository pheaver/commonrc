#!/bin/sh

# set keyboard repeat delay and rate
xset r rate 300 40

# Allows Ctrl+Alt+Backspace to restart Xorg
setxkbmap -option terminate:ctrl_alt_bksp

# make CapsLock behave like Ctrl:
setxkbmap -option ctrl:nocaps

# should remap Caps_Lock as Control_L
# xmodmap ~/.Xmodmap

# make short-pressed Ctrl behave like Escape:
# xcape -e 'Control_L=Escape'
