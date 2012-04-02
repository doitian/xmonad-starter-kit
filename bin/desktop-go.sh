#!/bin/sh

choise=$(desktop-list.rb | gpicker --name="DesktopGo" - | sed 's;/.*$;;')
if [ -n "$choise" ]; then
    wmctrl -s "$choise"
fi