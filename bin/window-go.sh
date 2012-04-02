#!/bin/sh

choise=$(wlist.rb | gpicker --name=WindowGo - | sed 's;/.*$;;')
echo "$choise"
if [ -n "$choise" ]; then
    wmctrl -i -a "$choise"
fi
