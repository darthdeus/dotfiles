#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar.log /tmp/polybar.log

# if type "xrandr"; then
#   for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#     MONITOR=$m polybar --reload example &
#   done
# else
#   polybar --reload example &
# fi


token=$(pass show github-polybar-notifications)
for m in $(polybar --list-monitors | cut -d":" -f1); do
    GITHUB_TOKEN="$token" MONITOR=$m polybar --reload example 2>&1 >>/tmp/polybar.log &
done

# polybar example >>/tmp/polybar.log 2>&1 &

# MONITOR=$(polybar --list-monitors | grep DP | cut -d: -f1) polybar example 2>&1 &

echo "Bars launched..."
