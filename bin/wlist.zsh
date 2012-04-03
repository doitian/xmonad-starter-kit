#!/usr/bin/env zsh

typeset -A workspaces
while read -A columns; do
  id=$columns[1]
  if [ "$columns[8]" = "N/A" ]; then
    shift 8 columns
  else
    shift 9 columns
  fi

  workspaces[$id]="${columns[*]}"
done < <(wmctrl -d)

first=1

while read id workspace_id resource_class host title; do
  workspace=${workspaces[$workspace_id]}
  win_class=$(echo $resource_class | sed 's/.*\.//')

  if [ "$win_class" != "Gpicker" ] && [ "$workspace" != "NSP" ]; then
    if [ -z "$first" ]; then
      echo -en '\0'
    fi
    echo -n "${id}/"
    echo -n "${workspace}" | sed 's/\// /g'
    echo -n '/'
    echo -n "${win_class} ${title}" | sed 's/\// /g'
    first=
  fi
done < <(wmctrl -x -l)
