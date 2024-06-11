#!/bin/bash

filePath="$HOME/.config/xmonad/lib/.layout.txt"

if [ -f "$filePath" ]; then
    layout=$(cat "$filePath")
    echo "$layout"
else
    echo "Layout: File not found."
fi
