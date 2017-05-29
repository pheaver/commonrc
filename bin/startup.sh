#!/bin/sh

set_kbd_config.sh

# todo don't do this on chromebook.
# which synclient >/dev/null && synclient TouchpadOff=1

# gkrellm &

nm-applet &

pasystray &

xfce4-power-manager

blueman-applet

dropbox start -i

# emacs --daemon &

