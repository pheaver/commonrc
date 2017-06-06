#!/bin/sh

echo 120 | sudo tee /sys/devices/platform/i8042/serio1/serio2/speed
echo 220 | sudo tee /sys/devices/platform/i8042/serio1/serio2/sensitivity
