#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
# killall -q polybar

# Launch the: pirateship; bar
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar pirateship 2>&1 | tee -a /tmp/polypirateship.log & disown

echo "Bars launched..."
