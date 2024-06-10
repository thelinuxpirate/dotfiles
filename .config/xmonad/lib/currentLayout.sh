#!/bin/bash

filePath="$HOME/.config/xmonad/lib/.layout.txt"

if [ -f "$filePath" ]; then
    layout=$(cat "$filePath")
    echo "Current Layout: $layout"
else
    echo "Layout file not found."
fi
