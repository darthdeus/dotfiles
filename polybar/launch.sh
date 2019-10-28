#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar.log /tmp/polybar.log
# polybar example >>/tmp/polybar.log 2>&1 &
polybar example 2>&1 &

echo "Bars launched..."
