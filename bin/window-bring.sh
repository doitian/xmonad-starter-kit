#!/bin/sh

choise=$(wlist.zsh | gpicker --name=WindowBring - | sed 's;/.*$;;')
echo "$choise"
if [ -n "$choise" ]; then
    wmctrl -i -R "$choise"
fi
