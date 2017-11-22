#!/bin/sh

arg="$1"

n=0

# just select the last card in the list (which I assume is the highest number, and probably the headset/headphones if plugged in).
for x in $(pactl list sinks short | cut -f 1); do
  n=$x
done

pactl set-sink-volume $n $arg
