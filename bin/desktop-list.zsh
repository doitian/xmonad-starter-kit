#!/usr/bin/env zsh

first=1

while read -A columns; do
  if [ "$columns[8]" = "N/A" ]; then
    shift 8 columns
  else
    shift 9 columns
  fi

  workspace_name="${columns[*]}"
  if [ "$workspace_name" != "NSP" ]; then
    if [ -z "$first" ]; then
      echo -en '\0'
    fi
    echo -n "$workspace_name"
    first=
  fi
done < <(wmctrl -d)
