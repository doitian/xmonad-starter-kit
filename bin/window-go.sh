#!/bin/sh

choise=$(wlist.zsh | gpicker --name=WindowGo - | sed 's;/.*$;;')
echo "$choise"
if [ -n "$choise" ]; then
    wmctrl -i -a "$choise"
fi
