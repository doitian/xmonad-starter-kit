#!/bin/sh

choise=$(wlist.rb | gpicker --name=WindowBring - | sed 's;/.*$;;')
echo "$choise"
if [ -n "$choise" ]; then
    wmctrl -i -R "$choise"
fi
