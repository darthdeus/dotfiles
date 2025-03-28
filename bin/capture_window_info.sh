#!/bin/bash
set -e

# Start logging i3 debug info
i3-msg -t command "debuglog on; shmlog on"

# Run the program and capture its window ID once it appears
$1 &
APP_PID=$!

# Short delay to let the window appear
sleep 1

# Find window ID associated with the process
WIN_ID=$(xdotool search --pid $APP_PID | head -1)

# Capture extensive window information
echo "=== WINDOW PROPERTIES ===" > window_info.log
xprop -id $WIN_ID >> window_info.log
echo "=== WINDOW INFO ===" >> window_info.log
xwininfo -id $WIN_ID >> window_info.log
echo "=== i3 WINDOW TREE ===" >> window_info.log
i3-msg -t get_tree | grep -C 10 "$(xprop -id $WIN_ID _NET_WM_NAME | cut -d\" -f2)" >> window_info.log

# Capture i3 debug log
sleep 2
i3-dump-log > i3_debug.log

# Clean up
i3-msg -t command "debuglog off; shmlog off"
wait $APP_PID || true

echo "Logs written to window_info.log and i3_debug.log"
