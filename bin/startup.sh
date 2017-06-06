#!/bin/sh

set_kbd_config.sh

which synclient >/dev/null && synclient TouchpadOff=1

# gkrellm &

nm-applet &

(killall pasystray; pasystray) &

xfce4-power-manager

(killall blueman-applet; blueman-applet) &

dropbox start -i

# emacs --daemon &
