#!/usr/bin/env zsh

while read -A columns; do
  id=$columns[1]
  if [ "$columns[8]" = "N/A" ]; then
    shift 8 columns
  else
    shift 9 columns
  fi
  if [ "${columns[*]}" = "$1" ]; then
    wmctrl -s $id
    exit 0
  fi
done < <(wmctrl -d)
